// ============================
// Fieldwork interactive slideshow
// ============================

// Map site names to arrays of photo objects {src, caption}
const siteImages = {
  "South Africa": [
    { src: "images/fieldwork/knp_1.jpg", caption: "Kruger National Park, South Africa" },
    { src: "images/fieldwork/knp_3.jpeg", caption: "Kruger National Park, South Africa" },
    { src: "images/fieldwork/knp_4.jpeg", caption: "Kruger National Park, South Africa" }
  ],
  "Florida": [
    { src: "images/fieldwork/abs_1.JPG", caption: "Archbold Biological Station, FL, USA" },
    { src: "images/fieldwork/abs_2.JPG", caption: "Archbold Biological Station, FL, USA" }, 
    { src: "images/fieldwork/abs_3.JPG", caption: "Archbold Biological Station, FL, USA" },
    { src: "images/fieldwork/abs_4.JPG", caption: "Archbold Biological Station, FL, USA" },
    { src: "images/fieldwork/abs_5.JPG", caption: "Archbold Biological Station, FL, USA" },
    { src: "images/fieldwork/abs_6.JPG", caption: "Archbold Biological Station, FL, USA" }
  ],
  "New Mexico": [
    { src: "images/fieldwork/jrn_1.jpg", caption: "Jornada LTER, NM, USA" },
    { src: "images/fieldwork/jrn_2.jpg", caption: "Jornada LTER, NM, USA" }, 
    { src: "images/fieldwork/jrn_3.jpg", caption: "Jornada LTER, NM, USA" },
    { src: "images/fieldwork/jrn_4.jpg", caption: "Jornada LTER, NM, USA" },
    { src: "images/fieldwork/jrn_5.jpg", caption: "Jornada LTER, NM, USA" },
    { src: "images/fieldwork/jrn_6.jpg", caption: "Jornada LTER, NM, USA" }
  ],
  "Minnesota": [
    { src: "images/fieldwork/cdr_1.JPG", caption: "Cedar Creek LTER, MN, USA" },
    { src: "images/fieldwork/cdr_2.JPG", caption: "Cedar Creek LTER, MN, USA" },
    { src: "images/fieldwork/cdr_3.JPG", caption: "Cedar Creek LTER, MN, USA" },
    { src: "images/fieldwork/cdr_4.JPG", caption: "Cedar Creek LTER, MN, USA" },
    { src: "images/fieldwork/cdr_5.JPG", caption: "Cedar Creek LTER, MN, USA" },
    { src: "images/fieldwork/cdr_6.JPG", caption: "Cedar Creek LTER, MN, USA" }
  ],
  "Colorado": [
    { src: "images/fieldwork/cper_1.jpg", caption: "Central Plains Experimental Range, CO, USA" },
    { src: "images/fieldwork/cper_2.jpg", caption: "Central Plains Experimental Range, CO, USA" },
    { src: "images/fieldwork/cper_3.jpeg", caption: "Central Plains Experimental Range, CO, USA" },
    { src: "images/fieldwork/cper_4.JPG", caption: "Central Plains Experimental Range, CO, USA" },
    { src: "images/fieldwork/cper_6.JPG", caption: "Central Plains Experimental Range, CO, USA" }
  ],
  "Kansas": [
    { src: "images/fieldwork/kpbs_1.JPG", caption: "Konza Prairie LTER, KS, USA" },
    { src: "images/fieldwork/kpbs_2.JPG", caption: "Konza Prairie LTER, KS, USA" },
    { src: "images/fieldwork/kpbs_3.JPG", caption: "Konza Prairie LTER, KS, USA" },
    { src: "images/fieldwork/kpbs_4.jpeg", caption: "Konza Prairie LTER, KS, USA" },
    { src: "images/fieldwork/kpbs_5.JPG", caption: "Konza Prairie LTER, KS, USA" },
    { src: "images/fieldwork/kpbs_6.JPG", caption: "Konza Prairie LTER, KS, USA" }
  ]
};

// Grab DOM elements
const markers = document.querySelectorAll(".marker");
const modal = document.getElementById("slideshow-modal");
const slideshowContent = document.getElementById("slideshow-content");
const closeBtn = document.querySelector(".close");
const prevBtn = document.querySelector(".prev");
const nextBtn = document.querySelector(".next");

let currentImages = [];
let currentIndex = 0;

// Open modal when marker clicked
markers.forEach(marker => {
  marker.addEventListener("click", () => {
    const location = marker.dataset.location;
    currentImages = siteImages[location];
    currentIndex = 0;
    showImage();
    modal.style.display = "block";
  });
});

// Display current image with caption
function showImage() {
  const imageObj = currentImages[currentIndex];
  slideshowContent.innerHTML = `
    <img src="${imageObj.src}" alt="Fieldwork photo">
    <p class="caption">${imageObj.caption}</p>
  `;
}

// Close modal
closeBtn.addEventListener("click", () => {
  modal.style.display = "none";
});

// Navigate slideshow
prevBtn.addEventListener("click", () => {
  currentIndex = (currentIndex - 1 + currentImages.length) % currentImages.length;
  showImage();
});

nextBtn.addEventListener("click", () => {
  currentIndex = (currentIndex + 1) % currentImages.length;
  showImage();
});

// Optional: click outside modal to close
window.addEventListener("click", e => {
  if (e.target === modal) modal.style.display = "none";
});
