# ---------------------------------------------------------------------------
# Saddle Transect: nightly Strava pull
#
# Runs in the GitHub Action. Writes:
#   data/saddle-transect/rides.geojson   every ride, geometry only, ends trimmed
#   data/saddle-transect/stories.json    rides tagged #saddletransect, with text
#   data/saddle-transect/checked.json    cache so we don't re-ask Strava every night
#   images/saddle-transect/<ride id>/    photos from tagged rides
#
# Needs env vars: STRAVA_CLIENT_ID, STRAVA_CLIENT_SECRET, STRAVA_REFRESH_TOKEN
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

if (!exists("%||%", baseenv())) `%||%` <- function(a, b) if (is.null(a)) b else a

# ---- settings (override any of these with an env var of the same name) ----
env <- function(x, default) { v <- Sys.getenv(x); if (nzchar(v)) v else default }

TAG          <- env("TAG", "#saddletransect")
TRIM_M       <- as.numeric(env("TRIM_M", "400"))      # meters cut off each end of every ride
MAX_DETAIL   <- as.integer(env("MAX_DETAIL", "80"))   # detail lookups per run (Strava rate limit)
RECHECK_DAYS <- as.integer(env("RECHECK_DAYS", "21")) # recent rides re-checked for a newly added tag
RIDE_TYPES   <- c("Ride", "GravelRide", "MountainBikeRide", "EBikeRide",
                  "EMountainBikeRide", "Velomobile", "Handcycle")
API          <- env("STRAVA_API_BASE", "https://www.strava.com/api/v3")
TOKEN_URL    <- env("STRAVA_TOKEN_URL", "https://www.strava.com/oauth/token")
OUT_DIR      <- env("OUT_DIR", "data/saddle-transect")
IMG_DIR      <- env("IMG_DIR", "images/saddle-transect")
NEW_TOKEN_FILE <- env("NEW_TOKEN_FILE", "")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(IMG_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- 1. access token ------------------------------------------------------
old_refresh <- Sys.getenv("STRAVA_REFRESH_TOKEN")
if (!nzchar(old_refresh)) stop("STRAVA_REFRESH_TOKEN is not set.")

tok <- request(TOKEN_URL) |>
  req_body_form(
    client_id     = Sys.getenv("STRAVA_CLIENT_ID"),
    client_secret = Sys.getenv("STRAVA_CLIENT_SECRET"),
    grant_type    = "refresh_token",
    refresh_token = old_refresh
  ) |>
  req_perform() |>
  resp_body_json()

# Strava can hand back a new refresh token and kill the old one.
# Save it right away so the workflow can update the GitHub secret.
if (!identical(tok$refresh_token, old_refresh)) {
  message("Strava issued a new refresh token.")
  if (nzchar(NEW_TOKEN_FILE)) writeLines(tok$refresh_token, NEW_TOKEN_FILE)
}

rate_limited <- FALSE
strava_get <- function(path, query = list()) {
  if (rate_limited) return(NULL)
  resp <- request(paste0(API, path)) |>
    req_auth_bearer_token(tok$access_token) |>
    req_url_query(!!!query) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
  status <- resp_status(resp)
  if (status == 429) {
    message("Hit Strava's rate limit. Saving progress; the rest happens next run.")
    rate_limited <<- TRUE
    return(NULL)
  }
  if (status == 404) return(NULL)
  if (status >= 400) stop("Strava returned ", status, " for ", path, ": ", resp_body_string(resp))
  resp_body_json(resp, simplifyVector = FALSE)
}

# ---- 2. every ride (summaries) -------------------------------------------
acts <- list()
page <- 1
repeat {
  batch <- strava_get("/athlete/activities", list(per_page = 200, page = page))
  if (is.null(batch) || length(batch) == 0) break
  acts <- c(acts, batch)
  page <- page + 1
}
if (rate_limited && length(acts) == 0) {
  message("Rate limited before any rides came back. Nothing written.")
  quit(status = 0)
}

sport <- vapply(acts, \(a) a$sport_type %||% a$type %||% "", "")
acts  <- acts[sport %in% RIDE_TYPES]
message(length(acts), " rides found.")

ride_id <- function(a) format(a$id, scientific = FALSE)
ids     <- vapply(acts, ride_id, "")
names(acts) <- ids

# ---- 3. which rides need a closer look? ----------------------------------
read_json_or <- function(path, default) {
  if (file.exists(path)) fromJSON(path, simplifyVector = FALSE) else default
}
checked <- read_json_or(file.path(OUT_DIR, "checked.json"), list())   # id -> TRUE/FALSE (tagged?)
stories <- read_json_or(file.path(OUT_DIR, "stories.json"), list())
names(stories) <- vapply(stories, \(s) s$id, "")

start_dates <- as.Date(substr(vapply(acts, \(a) a$start_date_local %||% "", ""), 1, 10))
recent      <- ids[!is.na(start_dates) & start_dates >= Sys.Date() - RECHECK_DAYS]

no_dirt  <- names(stories)[vapply(stories, \(s) is.null(s$pct_dirt), TRUE)]   # retry failed surface lookups
# Rides you asked for by hand: Strava links or IDs, separated by commas, spaces or new lines.
requested <- regmatches(env("RIDE_IDS", ""), gregexpr("[0-9]{6,}", env("RIDE_IDS", "")))[[1]]
unknown   <- setdiff(requested, ids)
if (length(unknown)) message("Not found among your rides (not a ride, or set to Only You?): ",
                             paste(unknown, collapse = ", "))
requested <- intersect(requested, ids)
if (length(requested)) message("Re-checking ", length(requested), " ride(s) you asked for.")

to_check <- union(union(setdiff(ids, names(checked)), recent), no_dirt)
to_check <- to_check[order(start_dates[match(to_check, ids)], decreasing = TRUE)]  # newest first
to_check <- c(requested, setdiff(to_check, requested))                            # yours go first
if (length(to_check) > MAX_DETAIL) {
  message(length(to_check), " rides to check; doing ", MAX_DETAIL, " now, the rest on later runs.")
  to_check <- head(to_check, max(MAX_DETAIL, length(requested)))
}

# ---- geometry helpers ----------------------------------------------------
# Google encoded polyline -> matrix of (lat, lng). Base R so the Action needs no extra packages.
decode_polyline <- function(s) {
  if (is.null(s) || !nzchar(s)) return(matrix(numeric(0), ncol = 2))
  b <- utf8ToInt(s) - 63L
  vals <- numeric(length(b)); n <- 0L; shift <- 0; result <- 0
  for (x in b) {
    result <- bitwOr(result, bitwShiftL(bitwAnd(x, 31L), shift))
    shift <- shift + 5
    if (x < 32L) {
      n <- n + 1L
      vals[n] <- if (bitwAnd(result, 1L)) -(bitwShiftR(result, 1L)) - 1 else bitwShiftR(result, 1L)
      shift <- 0; result <- 0
    }
  }
  d <- matrix(vals[seq_len(n)], ncol = 2, byrow = TRUE)   # deltas: lat, lng
  cbind(cumsum(d[, 1]), cumsum(d[, 2])) / 1e5
}

haversine <- function(lat1, lng1, lat2, lng2) {
  r <- pi / 180
  a <- sin((lat2 - lat1) * r / 2)^2 + cos(lat1 * r) * cos(lat2 * r) * sin((lng2 - lng1) * r / 2)^2
  2 * 6371000 * asin(sqrt(a))
}

# Belt and braces on top of Strava's privacy zones: drop the first and last TRIM_M meters.
trim_ends <- function(m) {
  if (nrow(m) < 3 || TRIM_M <= 0) return(m)
  step <- c(0, haversine(m[-nrow(m), 1], m[-nrow(m), 2], m[-1, 1], m[-1, 2]))
  cum  <- cumsum(step)
  m[cum > TRIM_M & cum < max(cum) - TRIM_M, , drop = FALSE]
}

# ---- 4. percent dirt (OpenStreetMap surface tags) ------------------------
# Strava doesn't report surface for activities, so: sample the ride every
# 50 m, find the nearest OSM road/trail within 25 m, and read its surface.
OVERPASS <- env("OVERPASS_URL", "https://overpass-api.de/api/interpreter")

PAVED <- c("paved", "asphalt", "concrete", "concrete:plates", "concrete:lanes",
           "paving_stones", "sett", "cobblestone", "unhewn_cobblestone",
           "metal", "wood", "chipseal", "bricks")
PAVED_HIGHWAYS <- c("motorway", "trunk", "primary", "secondary", "tertiary",
                    "unclassified", "residential", "service", "living_street",
                    "motorway_link", "trunk_link", "primary_link", "secondary_link",
                    "tertiary_link", "cycleway", "footway", "pedestrian")
DIRT_HIGHWAYS  <- c("track", "path", "bridleway")

way_surface <- function(tags) {
  s <- tags$surface
  if (!is.null(s)) return(if (s %in% PAVED) "paved" else "dirt")
  tt <- tags$tracktype
  if (!is.null(tt)) return(if (tt == "grade1") "paved" else "dirt")
  h <- tags$highway %||% ""
  if (h %in% DIRT_HIGHWAYS)  return("dirt")
  if (h %in% PAVED_HIGHWAYS) return("paved")
  NA_character_   # steps, construction, etc.: don't count
}

# Keep one point every `step` meters along the line.
resample <- function(m, step) {
  if (nrow(m) < 2) return(m)
  cum <- cumsum(c(0, haversine(m[-nrow(m), 1], m[-nrow(m), 2], m[-1, 1], m[-1, 2])))
  m[!duplicated(floor(cum / step)), , drop = FALSE]
}

pct_dirt <- function(m) {
  if (nrow(m) < 2) return(NULL)
  q <- resample(m, 150)
  s <- resample(m, 50)
  query <- sprintf("[out:json][timeout:120];way[highway](around:25,%s);out tags geom;",
                   paste(sprintf("%.5f,%.5f", q[, 1], q[, 2]), collapse = ","))
  res <- tryCatch(
    request(OVERPASS) |>
      req_user_agent("saddle-transect (courtcurrier.github.io)") |>
      req_body_form(data = query) |>
      req_timeout(180) |>
      req_retry(max_tries = 3) |>
      req_perform() |>
      resp_body_json(),
    error = \(e) { message("  surface lookup failed, will retry next run: ", conditionMessage(e)); NULL })
  if (is.null(res) || !length(res$elements)) return(NULL)

  # Every way -> its segments, with that way's surface class
  segs <- do.call(rbind, lapply(res$elements, \(w) {
    cls <- way_surface(w$tags)
    g <- w$geometry
    if (is.na(cls) || length(g) < 2) return(NULL)
    lat <- vapply(g, \(p) p$lat, 0); lon <- vapply(g, \(p) p$lon, 0)
    n <- length(lat)
    data.frame(lat1 = lat[-n], lon1 = lon[-n], lat2 = lat[-1], lon2 = lon[-1],
               dirt = cls == "dirt")
  }))
  if (is.null(segs) || !nrow(segs)) return(NULL)

  # Flat-earth meters around the ride; fine at this scale.
  lat0 <- mean(s[, 1]); kx <- 111320 * cos(lat0 * pi / 180); ky <- 110540
  px <- s[, 2] * kx; py <- s[, 1] * ky
  ax <- segs$lon1 * kx; ay <- segs$lat1 * ky
  bx <- segs$lon2 * kx; by <- segs$lat2 * ky
  dx <- bx - ax; dy <- by - ay; len2 <- pmax(dx^2 + dy^2, 1e-9)

  near_dirt <- rep(NA, length(px))
  for (i in seq_along(px)) {
    t  <- pmin(pmax(((px[i] - ax) * dx + (py[i] - ay) * dy) / len2, 0), 1)
    d2 <- (ax + t * dx - px[i])^2 + (ay + t * dy - py[i])^2
    j  <- which.min(d2)
    if (d2[j] <= 25^2) near_dirt[i] <- segs$dirt[j]
  }
  matched <- !is.na(near_dirt)
  if (mean(matched) < 0.5) return(NULL)   # too little of the ride matched a mapped way
  round(100 * mean(near_dirt[matched]))
}

# ---- 5. pull text + photos for tagged rides ------------------------------
strip_tag <- function(txt) {
  txt <- gsub(TAG, "", txt, ignore.case = TRUE, fixed = FALSE)
  trimws(gsub("[ \t]+\n", "\n", txt))
}

get_photos <- function(id, detail) {
  # The /photos endpoint gives every photo; it's not in Strava's public docs,
  # so fall back to the primary photo if it doesn't answer.
  ph <- strava_get(paste0("/activities/", id, "/photos"),
                   list(size = 1600, photo_sources = "true"))
  urls <- character(0)
  keys <- character(0)
  if (length(ph)) {
    for (p in ph) {
      u <- unlist(p$urls)
      if (length(u)) {
        urls <- c(urls, u[[length(u)]])
        keys <- c(keys, p$unique_id %||% as.character(length(keys) + 1))
      }
    }
  } else if (!is.null(detail$photos$primary$urls)) {
    u <- unlist(detail$photos$primary$urls)
    urls <- u[[length(u)]]
    keys <- detail$photos$primary$unique_id %||% "primary"
  }
  if (!length(urls)) return(list())

  dir.create(file.path(IMG_DIR, id), showWarnings = FALSE)
  out <- list()
  for (i in seq_along(urls)) {
    dest <- file.path(IMG_DIR, id, paste0(gsub("[^A-Za-z0-9_-]", "", keys[i]), ".jpg"))
    if (!file.exists(dest)) {
      ok <- tryCatch({
        request(urls[i]) |> req_perform(path = dest); TRUE
      }, error = \(e) { message("  photo download failed: ", conditionMessage(e)); FALSE })
      if (!ok) next
    }
    out[[length(out) + 1]] <- dest
  }
  out
}

for (id in to_check) {
  d <- strava_get(paste0("/activities/", id))
  if (rate_limited) break
  if (is.null(d)) next
  desc   <- d$description %||% ""
  tagged <- grepl(TAG, desc, ignore.case = TRUE)
  checked[[id]] <- tagged

  if (tagged) {
    message("  story: ", d$name)
    # Percent dirt only needs computing once per ride.
    dirt <- stories[[id]]$pct_dirt
    if (is.null(dirt)) {
      dirt <- pct_dirt(decode_polyline(d$map$polyline %||% d$map$summary_polyline))
      if (!is.null(dirt)) message("    ", dirt, "% dirt")
    }
    stories[[id]] <- list(
      id          = id,
      name        = d$name,
      date        = substr(d$start_date_local, 1, 10),
      miles       = round(d$distance / 1609.344, 1),
      climb_ft    = round((d$total_elevation_gain %||% 0) * 3.28084),
      moving_min  = round((d$moving_time %||% 0) / 60),
      elapsed_min = round((d$elapsed_time %||% 0) / 60),
      avg_mph     = round((d$average_speed %||% 0) * 2.23694, 1),   # Strava gives m/s, based on moving time
      location    = d$location_city %||% "",
      text        = strip_tag(desc),
      photos      = get_photos(id, d),
      strava_url  = paste0("https://www.strava.com/activities/", id)
    )
    stories[[id]]$pct_dirt <- dirt   # left out if the lookup failed; retried next run
  } else if (!is.null(stories[[id]])) {
    message("  tag removed: ", d$name)
    stories[[id]] <- NULL
  }
}

# Rides you deleted on Strava drop out of the stories too.
stories <- stories[names(stories) %in% ids]

# ---- 6. route lines -------------------------------------------------------
features <- list()
for (id in ids) {
  a <- acts[[id]]
  m <- trim_ends(decode_polyline(a$map$summary_polyline))
  if (nrow(m) < 2) next   # indoor ride, or too short once trimmed
  features[[length(features) + 1]] <- list(
    type = "Feature",
    properties = list(
      id    = id,
      name  = a$name,
      date  = substr(a$start_date_local, 1, 10),
      miles = round(a$distance / 1609.344, 1),
      story = !is.null(stories[[id]])
    ),
    geometry = list(
      type = "LineString",
      coordinates = lapply(seq_len(nrow(m)), \(i) round(c(m[i, 2], m[i, 1]), 5))  # GeoJSON is lng, lat
    )
  )
}

# ---- 7. write -------------------------------------------------------------
stories <- stories[order(vapply(stories, \(s) s$date, ""), decreasing = TRUE)]

write_json(list(type = "FeatureCollection", features = features),
           file.path(OUT_DIR, "rides.geojson"), auto_unbox = TRUE, digits = NA)
write_json(unname(stories), file.path(OUT_DIR, "stories.json"),
           auto_unbox = TRUE, pretty = TRUE, digits = NA)
write_json(checked, file.path(OUT_DIR, "checked.json"), auto_unbox = TRUE)

message("Wrote ", length(features), " routes and ", length(stories), " stories.")
