
    $(function() {
      var well = $('.well').first();
      function rehome() {
        var p = document.getElementById('shiny-notification-panel');
        if (p && p.previousElementSibling !== well[0]) well.after(p);
      }
      new MutationObserver(rehome).observe(document.body, {childList: true});
      rehome();

      // Show a busy state on the Run button, delayed so quick updates do not flicker
      var busyTimer = null;
      $(document).on('shiny:busy', function() {
        busyTimer = setTimeout(function() { $('#run').addClass('running').prop('disabled', true).text('Running...'); }, 250);
      });
      $(document).on('shiny:idle', function() {
        clearTimeout(busyTimer);
        $('#run').removeClass('running').prop('disabled', false).text('Run simulation');
        if (window.syncRunStale) window.syncRunStale();
      });

      // Save any plot exactly as shown (buttons carry the plot id and file name)
      $(document).on('click', '.dl-img', function() {
        var img = $('#' + $(this).data('target') + ' img')[0];
        var name = $(this).data('file') || 'plot.png';
        if (!img) return;
        fetch(img.src).then(function(r) { return r.blob(); }).then(function(blob) {
          var a = document.createElement('a');
          a.href = URL.createObjectURL(blob);
          a.download = name;
          document.body.appendChild(a); a.click(); a.remove();
          setTimeout(function() { URL.revokeObjectURL(a.href); }, 1000);
        });
      });

      // Click (or Enter / Space) on a draws tile asks the app for the enlarged plot
      function expandTile(el) { Shiny.setInputValue('expand_draw', $(el).data('id'), {priority: 'event'}); }
      $(document).on('click', '.draw-tile', function() { expandTile(this); });
      $(document).on('keydown', '.draw-tile', function(e) {
        if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); expandTile(this); }
      });

      // Save all the draws tiles as one image, three to a row
      $(document).on('click', '.dl-grid', function() {
        var imgs = $('#' + $(this).data('target') + ' .draw-tile:visible img').toArray();
        var name = $(this).data('file') || 'plots.png';
        if (!imgs.length) return;
        var cols = Math.min(3, imgs.length), w = imgs[0].naturalWidth, h = imgs[0].naturalHeight;
        var c = document.createElement('canvas');
        c.width = cols * w; c.height = Math.ceil(imgs.length / cols) * h;
        var ctx = c.getContext('2d'); ctx.fillStyle = '#fff'; ctx.fillRect(0, 0, c.width, c.height);
        imgs.forEach(function(im, i) { ctx.drawImage(im, (i % cols) * w, Math.floor(i / cols) * h, w, h); });
        c.toBlob(function(blob) {
          var a = document.createElement('a'); a.href = URL.createObjectURL(blob); a.download = name;
          document.body.appendChild(a); a.click(); a.remove();
          setTimeout(function() { URL.revokeObjectURL(a.href); }, 1000);
        });
      });

      // Assumption draws: show all, only varying, or only fixed cards, with a running count
      function applyDrawsFilter() {
        var tiles = $('.draw-tile'), nFixed = tiles.filter('[data-fixed="1"]').length;
        // The filter only means something when there are both fixed and varying assumptions
        var mixed = nFixed > 0 && nFixed < tiles.length;
        $('.seg-js').toggle(mixed);
        if (!mixed) $('.seg-js button').removeClass('on').first().addClass('on');
        var f = $('.seg-js .on').data('filter') || 'all', n = 0;
        tiles.each(function() {
          var fixed = $(this).attr('data-fixed') === '1', show = f === 'all' || (f === 'fixed' ? fixed : !fixed);
          $(this).toggle(show); if (show) n++;
        });
        $('.draws-count').text(mixed ? n + ' of ' + tiles.length + ' shown'
                               : tiles.length ? (nFixed ? 'All ' + tiles.length + ' assumptions are fixed' : 'All ' + tiles.length + ' assumptions are varying') : '');
      }
      $(document).on('click', '.seg-js button', function() {
        $(this).addClass('on').siblings().removeClass('on'); applyDrawsFilter();
      });
      $(document).on('shiny:value', function(e) { if (e.name === 'draws_grid') setTimeout(applyDrawsFilter, 50); });

      // Allow loading the same settings file twice in a row
      $(document).on('click', '#load_settings', function() { this.value = ''; });

      // Red outline and message on an assumption box with invalid values
      Shiny.addCustomMessageHandler('assumpErr', function(errs) {
        Object.keys(errs).forEach(function(id) {
          var msg = errs[id] || '';
          $('#' + id + '_err').text(msg);
          if (msg !== '') $('#card_' + id).addClass('open').find('.assump-toggle').attr('aria-expanded', 'true');
          $('#' + id + '_dist').closest('.well').toggleClass('assump-invalid', msg !== '');
        });
      });

      // A blocked run brings the first card with a problem into view and pulses it
      Shiny.addCustomMessageHandler('scrollToError', function(msg) {
        var card = $('.assump-invalid').first();
        if (!card.length) return;
        var sc = pageScroller(), r = card[0].getBoundingClientRect();
        var y = (sc ? sc.scrollTop : window.scrollY) + r.top - (sc ? sc.getBoundingClientRect().top : 0) - 70;
        if (sc) sc.scrollTo({top: Math.max(0, y), behavior: 'smooth'}); else window.scrollTo({top: Math.max(0, y), behavior: 'smooth'});
        card.removeClass('err-pulse'); void card[0].offsetWidth; card.addClass('err-pulse');
        card.find('input:visible').first().trigger('focus');
      });

      Shiny.addCustomMessageHandler('assumpWarn', function(w) {
        Object.keys(w).forEach(function(id) { $('#' + id + '_warn').text(w[id] || ''); });
      });

      // Cmd or Ctrl + Enter runs the simulation
      $(document).on('keydown', function(e) {
        if ((e.metaKey || e.ctrlKey) && e.key === 'Enter') {
          e.preventDefault();
          if (document.activeElement) document.activeElement.blur();
          setTimeout(function() { if (!$('#run').prop('disabled')) $('#run').click(); }, 60);
        }
      });

      // Copy text to the clipboard, with a fallback for pages where the clipboard API is blocked
      function copyText(text) {
        return new Promise(function(resolve) {
          function fallback() {
            var ta = document.createElement('textarea');
            ta.value = text; ta.style.position = 'fixed'; ta.style.opacity = 0;
            document.body.appendChild(ta); ta.select();
            var ok = false;
            try { ok = document.execCommand('copy'); } catch (e) {}
            ta.remove(); resolve(ok);
          }
          if (navigator.clipboard && window.isSecureContext) {
            navigator.clipboard.writeText(text).then(function() { resolve(true); }, fallback);
          } else { fallback(); }
        });
      }
      function flash(btn, text, success) {
        var b = btn.find('.fb-b'), msg = b.find('.fb-msg');
        (msg.length ? msg : b).text(text);
        b.find('.fb-icon').toggle(!!success);
        btn.addClass('copied');
        clearTimeout(btn.data('flashTimer'));
        btn.data('flashTimer', setTimeout(function() { btn.removeClass('copied'); }, 2450));  // 0.45s fade-in + 2s on screen
      }

      // Copy a table as tab-separated text, which pastes into Excel or Word as a table
      $(document).on('click', '.cite-btn', function() {
        var btn = $(this);
        copyText(String(btn.attr('data-cite'))).then(function(ok) { flash(btn, ok ? 'Copied!' : 'Copy failed', ok); });
      });
      $(document).on('click', '.copy-table', function() {
        var btn = $(this);
        var rows = $('#' + btn.data('target') + ' table tr').map(function() {
          return $(this).find('th, td').map(function() { return $(this).text().trim(); }).get().join('\t');
        }).get();
        if (!rows.length) { flash(btn, 'Nothing to copy yet'); return; }
        window.__lastCopied = rows.join('\n');
        copyText(window.__lastCopied).then(function(ok) { flash(btn, ok ? 'Copied!' : 'Copy failed', ok); });
      });

      // Shareable link: every setting goes into the address after the # sign
      var settingIds = ['a_bite', 'n_eip', 'm_dens', 'vec_comp', 'mort_a', 'mort_b', 'mort_s', 'growth_r', 'first_bite'];
      var settingFields = ['value', 'min', 'mode', 'max', 'mean', 'sd', 'shape1', 'shape2'];
      function topWin() { try { void window.top.location.href; return window.top; } catch (e) { return window; } }
      function settingsParams() {
        var p = new URLSearchParams();
        p.set('model', $('#mort_model').val()); p.set('structure', $('#structure').val());
        p.set('trials', $('#n_iter').val()); p.set('seed', $('#seed').val());
        settingIds.forEach(function(id) {
          p.set(id + '.dist', $('#' + id + '_dist').val());
          var src = $('#' + id + '_source').val();
          if (src) p.set(id + '.source', src);
          settingFields.forEach(function(f) {
            var v = $('#' + id + '_' + f).val();
            if (v !== undefined && v !== null && v !== '') p.set(id + '.' + f, v);
          });
        });
        (window.__corr || []).forEach(function(c, i) { p.set('corr.' + (i + 1), c); });
        return p;
      }
      $(document).on('click', '#copy_link', function() {
        var btn = $(this), w = topWin();
        var url = w.location.href.split('#')[0] + '#' + settingsParams().toString();
        window.__lastLink = url;
        try { w.history.replaceState(null, '', url); } catch (e) {}
        copyText(url).then(function(ok) { flash(btn, ok ? 'Copied!' : 'See address bar', ok); });
      });
      function sendHashSettings() {
        var h = topWin().location.hash.replace(/^#/, '');
        if (!h) return;
        var p = new URLSearchParams(h);
        if (!p.has('model')) return;
        var o = {}; p.forEach(function(v, k) { o[k] = v; });
        Shiny.setInputValue('url_settings', o, {priority: 'event'});
      }
      if (Shiny.shinyapp && Shiny.shinyapp.isConnected()) { sendHashSettings(); }
      else { $(document).one('shiny:connected', sendHashSettings); }

      // Hide or show the settings panel so the results can use the full width
      function setSidebar(collapsed) {
        $('.container-fluid').first().toggleClass('sidebar-collapsed', collapsed);
        $('#collapse_sidebar').attr('aria-expanded', String(!collapsed));
        $('#expand_sidebar').attr('aria-expanded', String(!collapsed));
        // Let the slide finish, then redraw plots at the new width and move keyboard focus to the arrow that is now visible
        setTimeout(function() {
          $(window).trigger('resize');
          $(collapsed ? '#expand_sidebar' : '#collapse_sidebar').trigger('focus');
        }, 380);
      }
      $(document).on('click', '#collapse_sidebar', function() { setSidebar(true); });
      $(document).on('click', '#expand_sidebar', function() { setSidebar(false); });

      // Cards whose values come from uploaded draws, and the correlation list (needed for the share link)
      Shiny.addCustomMessageHandler('uploadedCards', function(up) {
        window.__upCount = 0;
        Object.keys(up).forEach(function(id) {
          var msg = up[id] || '';
          if (msg !== '') window.__upCount++;
          $('#' + id + '_up').text(msg);
          $('#' + id + '_dist').closest('.well').toggleClass('assump-uploaded', msg !== '');
        });
        $(document).trigger('vc:linkstate');
        setTimeout(refreshAll, 50);
      });
      window.__corr = [];
      Shiny.addCustomMessageHandler('corrState', function(c) { window.__corr = c || []; $(document).trigger('vc:linkstate'); });

      // ---- Compact assumption cards, section jump bar, Advanced panel, Getting started ----
      var cardSections = {
        sec_transmission: ['a_bite', 'n_eip', 'm_dens', 'vec_comp'],
        sec_mortality: ['mort_a', 'mort_b', 'mort_s'],
        sec_population: ['growth_r', 'first_bite']
      };
      function fmtNum(v) { var n = parseFloat(v); return isNaN(n) ? '?' : String(parseFloat(n.toPrecision(3))); }
      function cardSummary(id) {
        var d = $('#' + id + '_dist').val(), f = function(k) { return fmtNum($('#' + id + '_' + k).val()); };
        if (d === 'Fixed') return 'Fixed at ' + f('value') + (id === 'growth_r' ? ' (calibrated, see card)' : '');
        if (d === 'Uniform') return 'Uniform, ' + f('min') + ' to ' + f('max');
        if (d === 'Triangular' || d === 'PERT') return d + ', ' + f('min') + ' to ' + f('max') + ', likeliest ' + f('mode');
        if (d === 'Beta') return 'Beta, ' + f('min') + ' to ' + f('max') + ', shapes ' + f('shape1') + ' and ' + f('shape2');
        if (d === 'Normal' || d === 'Lognormal') return d + ', mean ' + f('mean') + ', SD ' + f('sd') + ', limits ' + f('min') + ' to ' + f('max');
        return '';
      }
      function setTip(chip, text) { chip.attr('data-tip', text).attr('aria-label', $.trim(chip.clone().children().remove().end().text()) + ': ' + text); }
      function refreshCounts() {
        Object.keys(cardSections).forEach(function(sec) {
          var chip = $('.jump-chip[data-target="' + sec + '"]'), shown = $('#' + sec).is(':visible');
          chip.toggle(shown);
          if (!shown) return;
          if ($('#' + sec + ' .card-off').length) { $('#' + sec + ' .sec-count').text(''); chip.find('.jump-count').text(''); setTip(chip, 'Not used with synchronous emergence.'); return; }
          var vis = cardSections[sec].filter(function(id) { return $('#card_' + id).is(':visible'); });
          var varying = vis.filter(function(id) { return $('#' + id + '_dist').val() !== 'Fixed' || $('#card_' + id).hasClass('assump-uploaded'); });
          var n = varying.length, m = vis.length;
          chip.find('.jump-count').text(n + '/' + m);
          $('#' + sec + ' .sec-count').text(n + ' of ' + m + ' varying');
          setTip(chip, n === m ? 'All ' + m + ' assumptions here are drawn from a distribution, so each adds uncertainty to the forecast.'
                 : n === 0 ? 'None of these ' + m + ' assumptions are varying. Each is fixed at one value, so they add no uncertainty. Open a card and pick a distribution to vary one.'
                 : n + ' of ' + m + ' assumptions here are drawn from a distribution. The other ' + (m - n) + (m - n === 1 ? ' is' : ' are') + ' fixed at one value.');
        });
      }
      function refreshLinkStatus() {
        var n = (window.__corr || []).length, u = window.__upCount || 0, parts = [];
        if (n) parts.push(n + (n === 1 ? ' correlation' : ' correlations'));
        if (u) parts.push('uploaded draws for ' + u + (u === 1 ? ' assumption' : ' assumptions'));
        $('#link_status').text(parts.length ? parts.join(', ') : 'none set');
        var lc = $('.jump-chip[data-target="sec_linking"]');
        lc.find('.jump-count').text(parts.length ? 'on' : '');
        setTip(lc, parts.length ? 'Linking is on: ' + parts.join(', ') + '.' : 'Nothing is linked, so every assumption is drawn independently. Open this section to set correlations or upload joint draws.');
      }
      // With synchronous emergence the population cards are not used, so grey them out and explain on hover
      function refreshStructure() {
        var off = $('#structure').val() === 'synchronous';
        $('#sec_population .assump-card').each(function() {
          var c = $(this).toggleClass('card-off', off);
          if (off) c.removeClass('open').attr('data-tip', 'Synchronous emergence does not use a growth rate or a first-bite age.');
          else c.removeAttr('data-tip');
          c.find('.assump-toggle').attr({'aria-expanded': 'false', 'aria-disabled': String(off), tabindex: off ? -1 : 0});
        });
      }
      // Give every closed card the height of the tallest one; open cards keep their natural height
      function equalizeCards() {
        var cards = $('.assump-card').css('min-height', ''), max = 0;
        cards.each(function() {
          if (!$(this).hasClass('open') && this.offsetHeight > max) max = this.offsetHeight;
        });
        if (max) cards.css('min-height', max + 'px');
      }
      // The colour key only lists the states that at least one card is in right now
      function updateLegend() {
        var any = false;
        [['lg-edit', 'assump-differs'], ['lg-run', 'assump-changed'], ['lg-up', 'assump-uploaded']].forEach(function(p) {
          var on = $('.assump-card.' + p[1]).length > 0; any = any || on;
          $('.card-legend .' + p[0]).toggleClass('on', on);
        });
        $('.card-legend').toggleClass('on', any);
      }
      // Preset summary: how many inputs are drawn from a distribution now, and briefly how many were before
      var varyNow = null, varyWas = null, varyTimer = null, presetJustChanged = false;
      function updatePresetVary() {
        var vis = $('.assump-card:visible').not('.card-off'), n = 0;
        vis.each(function() {
          var id = this.id.replace('card_', '');
          if ($('#' + id + '_dist').val() !== 'Fixed' || $(this).hasClass('assump-uploaded')) n++;
        });
        var m = vis.length, el = $('#preset_vary');
        if (!m) return;
        if (presetJustChanged && varyNow !== null && n !== varyNow) {
          varyWas = varyNow; presetJustChanged = false;
          clearTimeout(varyTimer); varyTimer = setTimeout(function() { varyWas = null; updatePresetVary(); }, 9000);
        }
        varyNow = n;
        var html = '<b>' + n + ' of ' + m + '</b> inputs drawn from a distribution' +
                   (varyWas !== null ? ' <span class="was">(was ' + varyWas + ')</span>' : '');
        if (el.html() !== html) el.html(html);
      }
      $(document).on('shiny:inputchanged', function(e) { if (e.name === 'preset') presetJustChanged = true; });
      function refreshAll() {
        refreshStructure();
        $('.assump-card').each(function() {
          var id = this.id.replace('card_', '');
          $('#' + id + '_summary').text($(this).hasClass('assump-uploaded') ? 'From uploaded draws' : cardSummary(id));
        });
        refreshCounts(); refreshLinkStatus(); equalizeCards(); updateLegend(); updatePresetVary();
      }
      var refreshTimer = null;
      function scheduleRefresh() { clearTimeout(refreshTimer); refreshTimer = setTimeout(refreshAll, 120); }
      $(document).on('input change', '.assump-card :input', scheduleRefresh);
      $(document).on('shiny:inputchanged', function(e) {
        if (/_dist$|_value$|_min$|_max$|_mode$|_mean$|_sd$|_shape[12]$|^mort_model$|^structure$/.test(e.name)) scheduleRefresh();
      });
      $(document).on('vc:linkstate', refreshLinkStatus);
      setTimeout(refreshAll, 600); setTimeout(refreshAll, 2000);
      var eqTimer = null;
      $(window).on('resize', function() { clearTimeout(eqTimer); eqTimer = setTimeout(equalizeCards, 150); });
      $(document).on('shown.bs.tab', equalizeCards);

      // Open and close cards in place; several can stay open together
      // Clicking anywhere on the card header area toggles it; the open body and its controls do not
      $(document).on('click', '.assump-card', function(e) {
        var card = $(this);
        if (card.hasClass('card-off')) return;
        if (!$(e.target).closest('.assump-toggle').length &&
            $(e.target).closest('.assump-body, .edited-tools, a, :input, label').length) return;
        var open = !card.hasClass('open');
        card.toggleClass('open', open).find('.assump-toggle').attr('aria-expanded', String(open));
        if (open) keepCardInView(card[0]);
      });
      // While a card opens, ease the page down just enough to show its whole body
      // (its top stays visible if it is taller than the view)
      var scrollAnim = 0;
      function keepCardInView(el) {
        var sc = pageScroller(), r = el.getBoundingClientRect();
        var clip = el.querySelector('.assump-body-clip'), body = el.querySelector('.assump-body');
        var finalBottom = r.bottom + (clip.scrollHeight - body.offsetHeight);   // where the bottom will be once fully open
        var top = (sc ? sc.getBoundingClientRect().top : 0) + 52;               // clear the sticky jump bar
        var bottom = (sc ? sc.getBoundingClientRect().bottom : window.innerHeight) - 12;
        var dy = Math.min(finalBottom - bottom, r.top - top);                   // never push the card's top above the bar
        if (dy <= 0) return;
        var y0 = sc ? sc.scrollTop : window.scrollY;
        function set(y) { if (sc) sc.scrollTop = y; else window.scrollTo(0, y); }
        if (window.matchMedia('(prefers-reduced-motion: reduce)').matches) { set(y0 + dy); return; }
        var id = ++scrollAnim, t0 = null;
        requestAnimationFrame(function step(ts) {
          if (id !== scrollAnim) return;                                         // a newer scroll took over
          if (t0 === null) t0 = ts;
          var p = Math.min(1, (ts - t0) / 380);
          var e = p < 0.5 ? 4 * p * p * p : 1 - Math.pow(-2 * p + 2, 3) / 2;    // ease in and out
          set(y0 + dy * e);
          if (p < 1) requestAnimationFrame(step);
        });
      }
      $(document).on('click', '#expand_all', function() {
        $('.assump-card:not(.card-off)').addClass('open').find('.assump-toggle').attr('aria-expanded', 'true'); setLink(true);
      });
      $(document).on('click', '#collapse_all', function() {
        $('.assump-card').removeClass('open').find('.assump-toggle').attr('aria-expanded', 'false'); setLink(false);
      });

      // The Linking panel is advanced, so it starts closed
      function setLink(open) {
        $('#sec_linking').toggleClass('adv-open', open); $('#link_toggle').attr('aria-expanded', String(open));
      }
      $(document).on('click', '#eq_toggle', function() {
        var open = !$('#eq_fold').hasClass('adv-open');
        $('#eq_fold').toggleClass('adv-open', open); $(this).attr('aria-expanded', String(open));
      });
      $(document).on('click', '#link_toggle', function() { setLink(!$('#sec_linking').hasClass('adv-open')); });

      // Jump bar: scroll to a section (animated, instant if reduced motion is on) and highlight the current one
      function pageScroller() { return window.innerWidth >= 768 ? document.querySelector('.tab-content') : null; }
      function scrollToSection(id) {
        var el = document.getElementById(id); if (!el) return;
        var sc = pageScroller(), y0 = sc ? sc.scrollTop : window.scrollY;
        var y1 = Math.max(0, y0 + el.getBoundingClientRect().top - (sc ? sc.getBoundingClientRect().top : 0) - 50);
        function set(y) { if (sc) { sc.scrollTop = y; } else { window.scrollTo(0, y); } }
        if (window.matchMedia('(prefers-reduced-motion: reduce)').matches) { set(y1); return; }
        var t0 = null;
        function step(ts) {
          if (t0 === null) t0 = ts;
          var p = Math.min(1, (ts - t0) / 280), e = 1 - Math.pow(1 - p, 3);
          set(y0 + (y1 - y0) * e); if (p < 1) requestAnimationFrame(step);
        }
        requestAnimationFrame(step);
      }
      var forcedChip = null, forcedUntil = 0;
      $(document).on('click', '.jump-chip', function() {
        var t = $(this).data('target'); if (t === 'sec_linking') setLink(true);
        forcedChip = t; forcedUntil = Date.now() + 1500;      // light the chip at once, even if the page cannot scroll that far
        scrollToSection(t); spy();
      });
      var spyQueued = false;
      function spy() {
        spyQueued = false;
        var sc = pageScroller(), base = sc ? sc.getBoundingClientRect().top : 0, active = null;
        var secs = $('.assump-section:visible, .about-section:visible');
        secs.each(function() {
          if (active === null || this.getBoundingClientRect().top - base <= 70) active = this.id;
        });
        var atBottom = sc ? (sc.scrollTop + sc.clientHeight >= sc.scrollHeight - 2)
                          : (window.scrollY + window.innerHeight >= document.documentElement.scrollHeight - 2);
        if (atBottom && secs.length && sc && sc.scrollTop > 0) active = secs.last().attr('id');
        if (forcedChip && Date.now() < forcedUntil && $('#' + forcedChip).is(':visible')) active = forcedChip;
        $('.jump-chip').removeClass('active').filter('[data-target="' + active + '"]').addClass('active');
      }
      document.addEventListener('scroll', function() { if (!spyQueued) { spyQueued = true; requestAnimationFrame(spy); } }, true);
      setTimeout(spy, 700);

      if (window.matchMedia('(prefers-reduced-motion: reduce)').matches) $.fx.off = true;
      // Getting started: dismissal is remembered (when the browser allows it) and leaves a one-line link
      function setHowto(show) {
        if (show) { $('#howto').slideDown(150); $('#howto_open').hide(); try { localStorage.removeItem('vc_howto'); } catch (e) {} }
        else { $('#howto').slideUp(150); $('#howto_open').show(); try { localStorage.setItem('vc_howto', 'dismissed'); } catch (e) {} }
      }
      $(document).on('click', '.howto-close', function() { setHowto(false); });
      $(document).on('click', '#howto_open', function() { setHowto(true); });
      try { if (localStorage.getItem('vc_howto') === 'dismissed') { $('#howto').hide(); $('#howto_open').show(); } } catch (e) {}

      // Start over: drop any settings from the address, then reload the app
      Shiny.addCustomMessageHandler('startOver', function(msg) {
        try { var w = topWin(); w.history.replaceState(null, '', w.location.pathname + w.location.search); } catch (e) {}
        window.location.reload();
      });

      // Per card: a not-run-yet badge when it changed since the last run, a reset link when it differs from the preset
      Shiny.addCustomMessageHandler('cardStates', function(st) {
        // When most cards changed at once (a preset or model switch), one note replaces a badge on every card
        var nChanged = Object.keys(st).filter(function(id) { return st[id].changed; }).length, bulk = nChanged >= 4;
        Object.keys(st).forEach(function(id) {
          $('#' + id + '_dist').closest('.well')
            .toggleClass('assump-changed', !!st[id].changed && !bulk)
            .toggleClass('assump-differs', !!st[id].differs);
        });
        $('.bulk-note').toggleClass('on', bulk);
        updateLegend();
      });

      // The run message can arrive before the handler above is registered, so also read it from the status line
      $(document).on('shiny:value', function(e) {
        if (e.name === 'run_status' && /^Run \d/.test(String(e.value))) $('body').addClass('has-results');
      });
      $(document).on('click', '.empty-run', function() { $('#run').trigger('click'); });

      // When the results are out of date the server shows a note; mirror that on the Run button
      function syncRunStale() {
        var stale = $('#stale_note .stale-note').length > 0, b = $('#run');
        b.toggleClass('stale', stale);
        if (!b.hasClass('running')) b.text(stale ? 'Re-run simulation' : 'Run simulation');
      }
      window.syncRunStale = syncRunStale;
      // When the "Settings changed" note is removed, keep a copy that shrinks and fades instead of vanishing
      function fadeOutStaleNote(node) {
        var ghost = $(node).clone().removeClass('stale-note').addClass('stale-ghost').appendTo($('#stale_note'));
        var el = ghost[0], h = el.getBoundingClientRect().height;
        if (!el.animate || window.matchMedia('(prefers-reduced-motion: reduce)').matches) { ghost.remove(); return; }
        el.style.overflow = 'hidden';
        var anim = el.animate([
          {height: h + 'px', opacity: 1, marginTop: '8px', paddingTop: '5px', paddingBottom: '5px'},
          {height: '0px', opacity: 0, marginTop: '0px', paddingTop: '0px', paddingBottom: '0px'}
        ], {duration: 300, easing: 'ease', fill: 'forwards'});
        anim.onfinish = function() { ghost.remove(); };
        setTimeout(function() { ghost.remove(); }, 700);   // safety net if the animation never finishes (page in the background)
      }
      $(function() {
        var n = document.getElementById('stale_note');
        if (n) new MutationObserver(function(muts) {
          muts.forEach(function(m) {
            Array.prototype.forEach.call(m.removedNodes, function(node) {
              if (node.nodeType === 1 && node.classList.contains('stale-note') && !$('#stale_note .stale-note').length) fadeOutStaleNote(node);
            });
          });
          syncRunStale();
        }).observe(n, {childList: true, subtree: true});
      });

      // Getting started tucks itself away after the first run you start yourself (the app also runs once on load)
      var userRan = false, howtoAutoDone = false;
      $(document).on('click', '#run', function() { userRan = true; });
      $(document).on('vc:results', function() {
        if (!userRan || howtoAutoDone) return;
        howtoAutoDone = true;
        if ($('#howto').is(':visible')) { $('#howto').slideUp(250); $('#howto_open').show(); }
      });

      // Purple dot on result tabs when a run has produced new results you have not viewed yet
      var resultTabs = ['Forecast', 'Sensitivity', 'Assumption draws', 'Survival curves'];
      Shiny.addCustomMessageHandler('newResults', function(msg) {
        $('body').addClass('has-results');
        $(document).trigger('vc:results');
        // On phones the results sit below the settings, so bring them into view
        if (window.innerWidth < 768 && !window.__firstRunDone) { window.__firstRunDone = true; }
        else if (window.innerWidth < 768) { setTimeout(function() { $('#tabs')[0].scrollIntoView(); }, 400); }
        resultTabs.forEach(function(v) {
          var a = $('#tabs a[data-value="' + v + '"]');
          if (!a.parent().hasClass('active')) a.addClass('tab-new').attr({title: 'New results', 'aria-label': v + ', new results'});
        });
      });
      // Preset descriptions: size the box to the visible one so the controls below ease into place
      function sizePresetStack() {
        var st = $('#preset_stack'), on = st.find('.preset-pane.on');
        if (on.length) st.css('height', on.outerHeight(true) + 'px');
      }
      $(document).on('shiny:inputchanged', function(e) {
        if (e.name !== 'preset') return;
        $('#preset_stack .preset-pane').removeClass('on').filter('[data-preset="' + e.value + '"]').addClass('on');
        sizePresetStack();
      });
      if (window.ResizeObserver) {
        var ro = new ResizeObserver(sizePresetStack);
        $('#preset_stack .preset-pane').each(function() { ro.observe(this); });
      }
      setTimeout(sizePresetStack, 50); $(window).on('resize', sizePresetStack);
      setTimeout(function() { $('#preset_stack').addClass('animate'); }, 700);   // no easing for the first sizing at load

      // Tab switch: hide the pane you are leaving at once (Bootstrap keeps it visible for ~150 ms, stacked under
      // the new one, which showed as a flash of the old plots) and start fading the new pane in right away
      $(document).on('show.bs.tab', '#tabs a', function(e) {
        var np = $($(e.target).attr('href')), op = e.relatedTarget ? $($(e.relatedTarget).attr('href')) : $();
        op.addClass('leaving').removeClass('in');
        np.addClass('in');
      });
      $(document).on('shown.bs.tab', '#tabs a', function() { $('.tab-pane.leaving').removeClass('leaving'); });

      // Light / dark theme. The choice is remembered, and plots redraw in the new colours.
      function sendTheme() {
        if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('theme', document.documentElement.getAttribute('data-theme') || 'light');
      }
      $(document).on('shiny:connected', sendTheme);
      if (window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected()) sendTheme();
      $(document).on('click', '#theme_toggle', function() {
        var next = document.documentElement.getAttribute('data-theme') === 'dark' ? 'light' : 'dark';
        document.documentElement.setAttribute('data-theme', next);
        try { localStorage.setItem('vc_theme', next); } catch (e) {}
        sendTheme();
        // Shiny reports each plot's background and text colour when it resizes, which is what makes R redraw them
        setTimeout(function() { $(window).trigger('resize'); }, 60);
      });

      // Cross-fade between tabs (Bootstrap fades panes that carry the fade class)
      $('#tabs').closest('.tabbable').find('> .tab-content > .tab-pane').addClass('fade').filter('.active').addClass('in');
      $(document).on('shown.bs.tab', '#tabs a', function() {
        $(this).removeClass('tab-new').removeAttr('title').removeAttr('aria-label');
        // All tabs share one scroll area, so a position left on a long tab would carry over and show only the bottom of a short one
        var sc = pageScroller(); if (sc) sc.scrollTop = 0;
        setTimeout(spy, 50);
        setTimeout(function() { $(window).trigger('resize'); }, 80);   // plots on a tab that was hidden during a theme change redraw now
      });
    });
  
