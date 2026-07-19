(function () {
    // Close the dialog when the click lands on the backdrop, i.e. outside
    // the dialog's content.
    document.querySelectorAll('dialog').forEach(function (dlg) {
        dlg.addEventListener('click', function (e) {
            if (e.target === dlg) dlg.close();
        });
    });

    // Closing is handled by <form method="dialog"> inside each dialog.
    document.querySelectorAll('[data-dialog-open]').forEach(function (el) {
        el.addEventListener('click', function () {
            const dlg = document.getElementById(el.dataset.dialogOpen);
            if (dlg) dlg.showModal();
        });
    });

    document.querySelectorAll('.toggle-password').forEach(function (btn) {
        btn.addEventListener('click', function () {
            const input = btn.parentNode.querySelector('input');
            const show = input.type === 'password';
            input.type = show ? 'text' : 'password';
            btn.textContent = show ? btn.dataset.textHide : btn.dataset.textShow;
        });
    });

    document.querySelectorAll('.clipboard').forEach(function (btn) {
        btn.addEventListener('click', function () {
            const span = btn.children[0];
            const oldVal = span.textContent;
            navigator.clipboard.writeText(btn.dataset.copy).then(
                function () { span.textContent = btn.dataset.textCopied; },
                function () { span.textContent = btn.dataset.textCopyFailed; }
            ).finally(function () {
                window.setTimeout(function () { span.textContent = oldVal; }, 1000);
            });
        });
    });

    const qr_el = document.getElementById('qr-invite-page');
    if (window.QRCode && qr_el) {
        new QRCode(qr_el, {
            text: qr_el.dataset.link || document.location.href,
            addQuietZone: true
        });
        const qr_button = document.getElementById('qr-button-container');
        if (qr_button) qr_button.hidden = false;
    }

    // Show only the client cards matching the visitor's platform (apps.html).
    if (!window.platform) return;
    const family = platform.os.family || '';
    let friendly, cls;
    switch (family) {
        case 'Ubuntu': case 'Linux': case 'Fedora': case 'Red Hat': case 'SuSE':
            friendly = family + ' (Linux)'; cls = 'linux'; break;
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
            if (family.startsWith('Windows')) { friendly = 'Windows'; cls = 'windows'; }
            else { friendly = family; cls = family.toLowerCase(); }
    }
    if (!friendly) return;

    const cards = document.getElementsByClassName('client-card');
    if (!document.querySelector('.client-card .client-platform-badge-' + cls)) {
        document.querySelectorAll('.client-card .client-platform-badge').forEach(function (badge) {
            badge.classList.replace('badge-info', 'badge-secondary');
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
            if (badge.classList.contains('client-platform-badge-' + cls)) {
                badge.classList.replace('badge-info', 'badge-success');
                has_platform = true;
            } else {
                badge.classList.replace('badge-info', 'badge-secondary');
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
        show_all.hidden = false;
        const btn = document.getElementById('show-all-clients-button');
        if (btn) btn.addEventListener('click', function (e) {
            e.preventDefault();
            for (const card of cards) card.hidden = false;
            show_all.hidden = true;
        });
    }
})();
