// ============================
// Fieldwork interactive slideshow
// ============================

// Map site names to arrays of photo objects {src, caption}
const siteImages = {
  "South Africa": [
    { src: "images/fieldwork/knp_1.jpg", caption: "Savanna research plot in South Africa" },
    { src: "images/fieldwork/knp_2.jpg", caption: "Measuring soil carbon stocks" }
  ],
  "Florida": [
    { src: "images/fieldwork/abs_1.jpg", caption: "Prescribed burn in Florida flatwoods" },
    { src: "images/fieldwork/abs_2.jpg", caption: "Long-term vegetation monitoring" }
  ],
  "New Mexico": [
    { src: "images/fieldwork/jrn_1.jpg", caption: "Desert grassland site, New Mexico" },
    { src: "images/fieldwork/jrn_2.jpg", caption: "Soil sampling in arid ecosystems" }
  ],
  "Minnesota": [
    { src: "images/fieldwork/cdr_1.jpg", caption: "Prairie plot in Minnesota" },
    { src: "images/fieldwork/cdr_2.jpg", caption: "Root biomass measurement" }
  ],
  "Colorado": [
    { src: "images/fieldwork/cper_1.jpg", caption: "High elevation grassland in Colorado" },
    { src: "images/fieldwork/cper_2.jpg", caption: "Tracking plant recovery after fire" }
  ],
  "Kansas": [
    { src: "images/fieldwork/kpbs_1.jpg", caption: "Tallgrass prairie, Kansas" },
    { src: "images/fieldwork/kpbs_2.jpg", caption: "Sampling above- and belowground biomass" }
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
