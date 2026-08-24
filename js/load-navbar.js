// Function required by Google Translate SDK
function googleTranslateElementInit() {
    new google.translate.TranslateElement({
        pageLanguage: 'en'
    }, 'google_translate_element');
}

// Fetch and inject navbar.html
fetch('navbar.html')
    .then(response => response.text())
    .then(htmlContent => {
        document.getElementById('navbar-placeholder').innerHTML = htmlContent;

        // Get current filename (e.g., "about-me.html" or "index.html")
        let currentPage = window.location.pathname.split('/').pop();
        if (!currentPage || currentPage === "") currentPage = 'index.html';

        // Highlight active link
        const activeLink = document.querySelector(`.navbar-nav a[data-page="${currentPage}"]`);
        if (activeLink) {
            activeLink.classList.add('active');
        }

        // Display current page title in brand link if not homepage
        const brandTitle = document.getElementById('nav-brand-title');
        if (brandTitle && currentPage !== 'index.html' && activeLink) {
            brandTitle.textContent = activeLink.textContent;
        }

        // Load Google Translate script dynamically after navbar HTML is inserted
        const translateScript = document.createElement('script');
        translateScript.src = "https://translate.google.com/translate_a/element.js?cb=googleTranslateElementInit";
        document.body.appendChild(translateScript);
    });