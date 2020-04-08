function onDrawerToggle() {
    let drawer = document.getElementById('drawer');
    if( drawer.classList.contains('mdc-drawer--open') ) {
        drawer.classList.remove('mdc-drawer--open');
    } else {
        drawer.classList.add('mdc-drawer--open');
    }
}

// Get the modal
let modalImages = function() { 
    var modal = document.getElementById("modal");

    var modalImg = document.getElementById("modal-image");
    var captionText = document.getElementById("modal-caption");

    var elements = document.getElementsByClassName("book-content")[0].getElementsByTagName("img");
    for(let index = 0; index < elements.length; ++index ) {
        let img = elements[index];
        img.onclick = function(){
            modal.style.display = "block";            
            captionText.innerHTML = this.alt;
            modalImg.style['background-image'] = 'url("' + this.src + '")';
        }
    }

    // Get the <span> element that closes the modal
    let span = document.getElementsByClassName("modal-close")[0];

    // When the user clicks on <span> (x), close the modal
    span.onclick = function() { modal.style.display = "none" };
};

modalImages();

// code language highlighting
hljs.initHighlightingOnLoad();

// navigation
function loadPage(event, page) {
    let uri = page || this.href;
    
    fetch(uri)
    .then(response => response.text())
    .then(text => {
        const parser = new DOMParser();
        const htmlDocument = parser.parseFromString(text, 'text/html');
        const page = htmlDocument.documentElement.querySelector('.book-content');
        const appContent = document.querySelector(".mdc-drawer-app-content");
        const currentPage = appContent.querySelector('.book-content') ;
        if(page) {
            appContent.removeChild(currentPage);
            let child = appContent.appendChild(page);
            child.querySelectorAll('pre code').forEach((block) => {
                hljs.highlightBlock(block);
              });
            history.pushState({ page: uri}, document.title, uri);        
        } else {
            const innerHtml = htmlDocument.documentElement.querySelector('body').innerHTML;
            currentPage.innerHTML = innerHtml;
        }      
    });

    event.preventDefault(); 
    return false; 
}

function interceptSummaryLinks() {
    document.querySelectorAll('aside a').forEach( _ => _.onclick = loadPage);
    window.onpopstate = function (event) {
        this.loadPage(event, window.location);       
    }
} 

interceptSummaryLinks();
    