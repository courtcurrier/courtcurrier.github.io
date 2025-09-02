const markers = document.querySelectorAll(".marker");
const modal = document.getElementById("slideshow-modal");
const content = document.getElementById("slideshow-content");
const closeBtn = document.querySelector(".close");
const prevBtn = document.querySelector(".prev");
const nextBtn = document.querySelector(".next");

let currentImages = [];
let currentIndex = 0;

// Define your location images
const locationPhotos = {
  location1: ["images/fieldwork_photos/location1_img1.jpg", "images/fieldwork_photos/location1_img2.jpg"],
  location2: ["images/fieldwork_photos/location2_img1.jpg", "images/fieldwork_photos/location2_img2.jpg"]
};

// Show modal with selected images
markers.forEach(marker => {
  marker.addEventListener("click", () => {
    const loc = marker.dataset.location;
    currentImages = locationPhotos[loc];
    currentIndex = 0;
    showImage();
    modal.style.display = "block";
  });
});

function showImage() {
  content.innerHTML = `<img src="${currentImages[currentIndex]}" alt="Fieldwork photo">`;
}

// Navigate
prevBtn.addEventListener("click", () => {
  currentIndex = (currentIndex - 1 + currentImages.length) % currentImages.length;
  showImage();
});
nextBtn.addEventListener("click", () => {
  currentIndex = (currentIndex + 1) % currentImages.length;
  showImage();
});

// Close modal
closeBtn.addEventListener("click", () => {
  modal.style.display = "none";
});

