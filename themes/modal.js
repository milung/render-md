// Get the modal
let modalImages = function() { 
    var modal = document.getElementById("modal");

    var modalImg = document.getElementById("modal-img");
    var captionText = document.getElementById("modal-caption");

    var elements = document.getElementsByClassName("section-body")[0].getElementsByTagName("img");
    for(let index = 0; index < elements.length; ++index ) {
        let img = elements[index];
        img.onclick = function(){
            modal.style.display = "block";
            modalImg.src = this.src;
            captionText.innerHTML = this.alt;
        }
    }

    // Get the <span> element that closes the modal
    let span = document.getElementsByClassName("modal-close")[0];

    // When the user clicks on <span> (x), close the modal
    span.onclick = function() { modal.style.display = "none" };
};

modalImages();
    