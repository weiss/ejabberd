(function () {
    // Close any <dialog> when the user clicks the backdrop (outside the article).
    document.querySelectorAll('dialog').forEach(function (dlg) {
        dlg.addEventListener('click', function (e) {
            if (e.target === dlg) dlg.close();
        });
    });

    // <dialog> open triggers — close is handled by <form method="dialog"> inside the dialog.
    document.querySelectorAll('[data-dialog-open]').forEach(function (el) {
        el.addEventListener('click', function () {
            const dlg = document.getElementById(el.getAttribute('data-dialog-open'));
            if (dlg && typeof dlg.showModal === 'function') dlg.showModal();
        });
    });

    // QR code (only on pages that include qrcode.min.js and have a target element).
    const qr_el = document.getElementById('qr-invite-page');
    if (window.QRCode && qr_el) {
        const link = qr_el.getAttribute('data-link') || document.location.href;
        new QRCode(qr_el, { text: link, addQuietZone: true });
        const qr_button = document.getElementById('qr-button-container');
        if (qr_button) {
            qr_button.classList.remove('hidden');
            qr_button.classList.add('qr-visible');
        }
    }

    // Clipboard buttons.
    document.querySelectorAll('.clipboard').forEach(function (btn) {
        btn.addEventListener('click', function () {
            const span = btn.children[0];
            const oldVal = span.innerText;
            navigator.clipboard.writeText(btn.getAttribute('data-copy')).then(
                function () { span.innerText = btn.getAttribute('data-text-copied'); },
                function () { span.innerText = btn.getAttribute('data-text-copy-failed'); }
            ).finally(function () {
                window.setTimeout(function () { span.innerText = oldVal; }, 1000);
            });
        });
    });

    // Platform-aware filtering of client cards (apps.html). Uses platform.min.js.
    if (!window.platform) return;
    let friendly = null, cls = null;
    switch (platform.os.family) {
        case 'Ubuntu': case 'Linux': case 'Fedora': case 'Red Hat': case 'SuSE':
            friendly = platform.os.family + ' (Linux)'; cls = 'linux'; break;
        case 'Linux aarch64':
            friendly = 'Linux mobile'; cls = 'linux'; break;
        case 'Haiku R1':
            friendly = 'Haiku'; cls = 'haiku'; break;
        case 'Windows Phone':
            friendly = 'Windows Phone'; cls = 'windows-phone'; break;
        case 'OS X':
            if (navigator.maxTouchPoints > 1) { friendly = 'iPadOS'; cls = 'ipados'; }
            else { friendly = 'macOS'; cls = 'macos'; }
            break;
        default:
            if (platform.os.family.startsWith('Windows')) { friendly = 'Windows'; cls = 'windows'; }
            else { friendly = platform.os.family; cls = friendly.toLowerCase(); }
    }
    if (!friendly || !cls) return;

    const cards = document.getElementsByClassName('client-card');
    const has_match = document.querySelectorAll('.client-card .client-platform-badge-' + cls).length > 0;
    if (!has_match) {
        document.querySelectorAll('.client-card .client-platform-badge').forEach(function (b) {
            b.classList.remove('badge-info');
            b.classList.add('badge-secondary');
        });
        return;
    }
    for (const card of cards) {
        if (card.classList.contains('app-platform-' + cls)) {
            card.classList.add('supported-platform');
        } else if (!card.classList.contains('app-platform-web')) {
            card.hidden = true;
        }
        let has_platform = false;
        card.querySelectorAll('.client-platform-badge').forEach(function (badge) {
            badge.classList.remove('badge-info');
            if (badge.classList.contains('client-platform-badge-' + cls)) {
                badge.classList.add('badge-success');
                has_platform = true;
            } else {
                badge.classList.add('badge-secondary');
            }
        });
        if (!has_platform) {
            const cta = card.querySelector('a.button');
            if (cta) cta.classList.add('secondary');
        }
    }
    const show_all = document.getElementById('show-all-clients-button-container');
    if (show_all) {
        const name_el = show_all.querySelector('.platform-name');
        if (name_el) name_el.textContent = friendly;
        show_all.classList.remove('hidden');
        const btn = document.getElementById('show-all-clients-button');
        if (btn) btn.addEventListener('click', function (e) {
            for (const card of cards) card.hidden = false;
            show_all.hidden = true;
            e.preventDefault();
        });
    }
})();
