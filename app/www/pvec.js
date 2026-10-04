// ---- Results gate: nothing from a run shows (a skeleton is shown instead) until the Run button's bar has finished, on the first
// load as well. PV_GATE.after(fn) runs fn straight away, or once the gate opens; the draw-in animations and number count-ups use it
// so they happen in view, after the reveal, not underneath the skeleton.
window.PV_GATE = (function () {
  var q = [], g = {on: true};
  document.body.classList.add('pv-gated');
  g.after = function (fn) { if (!g.on) fn(); else q.push(fn); };
  g.set = function (on) {
    if (on === g.on) return;
    g.on = on; document.body.classList.toggle('pv-gated', on);
    if (!on) {
      var f = q.splice(0);
      requestAnimationFrame(function () { requestAnimationFrame(function () { f.forEach(function (fn) { try { fn(); } catch (e) {} }); }); });
    }
  };
  return g;
})();


    $(function() {
      var dock = $('.run-dock').first();
      function rehome() {
        var p = document.getElementById('shiny-notification-panel');
        if (p && dock.length && p.previousElementSibling !== dock[0]) dock.after(p);
      }
      new MutationObserver(rehome).observe(document.body, {childList: true});
      rehome();

      // Run button progress. The fill is animated on the page so it stays visible for at least
      // RUN_MIN_MS even when the simulation itself finishes almost instantly.
      var RUN_MIN_MS = 1800, run = null;
      function runPct() {
        var n = $('.shiny-progress-notification').filter(function() { return /Running trials/.test($(this).text()); });
        var bar = n.find('.progress-bar')[0];
        return bar ? Math.max(0, Math.min(100, parseFloat(bar.style.width) || 0)) : null;
      }
      // Only the trial bar is folded into the button; other progress bars (e.g. the model check) still show
      function tagProgress() {
        $('.shiny-progress-notification').each(function() {
          if (/Running the comparison/.test($(this).text())) $(this).addClass('keep-progress');
        });
      }
      // The Run button's text lives in a span. "Run simulation" and "Run new simulation" share one structure, so "new" can slide
      // in between "Run" and "simulation" (and back out); progress text simply replaces it.
      function setRunLabel(text) {
        var b = $('#run'), lab = b.find('.run-label');
        if (!lab.length) { b.empty().append('<span class="run-label"></span>'); lab = b.find('.run-label'); }
        if (text === 'Run simulation' || text === 'Run new simulation') {
          if (!lab.find('.new-part').length) lab.html('Run <span class="new-part">new&nbsp;</span>simulation');
          lab.toggleClass('is-new', text === 'Run new simulation');
        } else {
          lab.removeClass('is-new').text(text);
        }
      }
      function setRun(pct) {
        var b = $('#run'), el = b[0];
        if (!el) return;
        el.style.setProperty('--p', pct + '%');
        var label = 'Running... ' + Math.round(pct) + '%';
        setRunLabel(label, false);
      }
      function stepRun() {
        if (!run) return;
        var timePct = Math.min(1, (performance.now() - run.t0) / RUN_MIN_MS) * 100;
        if (run.done && timePct >= 100) { finishRun(); return; }
        // While R is working, never run ahead of the real progress (or past 95%) until it reports done
        var shown = run.done ? timePct : Math.min(timePct, 95, runPct() || timePct * 0.5);
        setRun(shown);
      }
      // The status line under the button waits for the fill to finish, then shows the latest message
      var held = {};   // values of run_status and scenario_list wait until the fill in the Run button has finished
      $(document).on('shiny:value', function(e) {
        if ((e.name === 'run_status' || e.name === 'scenario_list') && run) { held[e.name] = e.value; e.preventDefault(); }
      });
      // New status text fades in over the old text, which fades out underneath
      function showStatus(msg) {
        var el = $('#run_status'), box = el.parent(), old = el.text();
        if (old && old !== msg) {
          var ghost = $('<div class="status-ghost" aria-hidden="true">').text(old).appendTo(box);
          setTimeout(function() { ghost.remove(); }, 1300);
        }
        el.text(msg).removeClass('status-in'); void el[0].offsetWidth; el.addClass('status-in');
      }
      function finishRun() {
        clearInterval(run.timer); run = null;
        if (held.run_status !== undefined) { showStatus(held.run_status); delete held.run_status; }
        if (held.scenario_list !== undefined) { Shiny.renderContent($('#scenario_list')[0], held.scenario_list); delete held.scenario_list; }
        var b = $('#run');
        b.removeClass('running').prop('disabled', false); setRunLabel('Run simulation', false);
        if (b[0]) b[0].style.removeProperty('--p');
        if (window.syncRunStale) window.syncRunStale();
        PV_GATE.set(false);
      }
      function startRunUI() {
        if (run) return;
        PV_GATE.set(true);
        run = {t0: performance.now(), done: false};
        // Wait a tick so Shiny registers the click before the button is disabled
        setTimeout(function() { $('#run').addClass('running').prop('disabled', true); setRun(0); run.timer = setInterval(stepRun, 30); }, 0);
      }
      $(document).on('click', '#run', startRunUI);
      // The app runs once by itself when the page opens, so the same bar (and skeleton) covers that first run
      startRunUI();
      setTimeout(function() { if (!run) PV_GATE.set(false); }, 15000);      // safety: never leave the results hidden if no run was under way
      $(document).on('shiny:idle', function() {
        if (run) run.done = true;
        else if (window.syncRunStale) window.syncRunStale();
      });
      new MutationObserver(tagProgress).observe(document.body, {childList: true, subtree: true});

      // Save any plot exactly as shown (buttons carry the plot id and file name)
      $(document).on('click', '.dl-img', function(e) {
        e.preventDefault();
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
      function openDriver(el) { Shiny.setInputValue('expand_driver', $(el).data('id'), {priority: 'event'}); }
      $(document).on('click', '.driver-tile', function() { openDriver(this); });
      $(document).on('keydown', '.driver-tile', function(e) {
        if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); openDriver(this); }
      });
      // Source links on the About tab: jump to the assumption card (opening it) or to another tab
      function showTab(name) { $('#tabs a[data-value="' + name + '"]').tab('show'); }
      $(document).on('click', '.src-link', function() {
        var card = $(this).data('card'), tab = $(this).data('tab');
        if (tab) { showTab(tab); return; }
        showTab('Define assumptions');
        setTimeout(function() {
          var c = $('#card_' + card);
          if (!c.length) return;
          if (!c.hasClass('open') && !c.hasClass('card-off')) c.find('.assump-toggle').first().trigger('click');
          c[0].scrollIntoView({behavior: 'smooth', block: 'center'});
          c.addClass('card-flash'); setTimeout(function() { c.removeClass('card-flash'); }, 2200);
        }, 450);
      });
      $(document).on('click', '.rc-rerun', function(e) { e.preventDefault(); $('#run').click(); });
      $(document).on('keydown', '.draw-tile', function(e) {
        if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); expandTile(this); }
      });

      // Save all the draws tiles as one image, three to a row
      $(document).on('click', '.dl-grid', function(e) {
        e.preventDefault();
        var imgs = $('#' + $(this).data('target') + ' .draw-tile:visible img, #' + $(this).data('target') + ' .combine-img:visible > .shiny-plot-output img').toArray();
        var name = $(this).data('file') || 'plots.png';
        if (!imgs.length) return;
        var cols = Math.min($(this).data('target') === 'surv_wrap' ? 2 : 3, imgs.length), w = imgs[0].naturalWidth, h = imgs[0].naturalHeight;
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
        applyDrawsSort();
      }
      // Order the cards as drawn, or with the biggest effect on Ct first (fixed ones last)
      function applyDrawsSort(animate) {
        var grid = $('.draws-grid').first(), by = $('.seg-sort .on').data('sort') || 'default';
        var tiles = grid.children('.draw-tile').toArray(), first = [];
        if (animate) first = tiles.map(function(t) { return t.offsetParent ? t.getBoundingClientRect() : null; });
        tiles.slice().sort(function(a, b) {
          if (by === 'effect') {
            var d = parseFloat($(b).attr('data-effect')) - parseFloat($(a).attr('data-effect'));
            if (d) return d;
          }
          return parseFloat($(a).attr('data-order')) - parseFloat($(b).attr('data-order'));
        }).forEach(function(t) { grid.append(t); });
        // Slide each card from where it was to where it now sits
        if (animate && tiles[0] && tiles[0].animate && !window.matchMedia('(prefers-reduced-motion: reduce)').matches) {
          tiles.forEach(function(t, i) {
            var f = first[i]; if (!f || !t.offsetParent) return;
            var n = t.getBoundingClientRect(), dx = f.left - n.left, dy = f.top - n.top;
            if (dx || dy) t.animate([{transform: 'translate(' + dx + 'px,' + dy + 'px)', zIndex: 5}, {transform: 'none', zIndex: 5}],
                                    {duration: 550, easing: 'cubic-bezier(.4,0,.2,1)'});
          });
        }
      }
      $(document).on('click', '.seg-sort button', function() {
        $(this).addClass('on').siblings().removeClass('on'); applyDrawsSort(true);
      });
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
        p.set('model', $('input[name=mort_model]:checked').val()); p.set('structure', $('input[name=structure]:checked').val());
        p.set('trials', String($('#n_iter').val()).replace(/,/g, '')); p.set('seed', $('#seed').val());
        p.set('temp', $('#temp_delta').val()); p.set('temp.on', $('#temp_on').prop('checked') ? 'TRUE' : 'FALSE'); p.set('temp.curve', $('#temp_curve').val()); p.set('temp.ref', $('#temp_ref').val()); p.set('temp.pe', $('#temp_pe').val()); p.set('temp.pm', $('#temp_pm').val()); p.set('temp.pa', $('#temp_pa').val());
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
        var off = $('input[name=structure]:checked').val() === 'synchronous';
        $('#sec_population .assump-card').each(function() {
          var c = $(this).toggleClass('card-off', off);
          if (off) c.removeClass('open').attr('data-tip', 'Synchronous emergence does not use a growth rate or a first-bite age.');
          else c.removeAttr('data-tip');
          c.find('.assump-toggle').attr({'aria-expanded': 'false', 'aria-disabled': String(off), tabindex: off ? -1 : 0});
        });
      }
      // Give every closed card the height of the tallest one; open cards keep their natural height
      function equalizeCards() {
        var cards = $('.assump-card').css('min-height', ''), rows = {};
        // Match heights only within a row of closed cards, so a short card is not stretched to the tallest card on the page
        cards.each(function() {
          if ($(this).hasClass('open') || !this.offsetParent) return;
          var key = $(this).parent()[0].id + '|' + this.offsetTop;
          (rows[key] = rows[key] || []).push(this);
        });
        Object.keys(rows).forEach(function(k) {
          var els = rows[k], max = Math.max.apply(null, els.map(function(el) { return el.offsetHeight; }));
          if (els.length > 1) els.forEach(function(el) { el.style.minHeight = max + 'px'; });
        });
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
      $(document).on('click', '#link_toggle', function() {
        var open = !$('#sec_linking').hasClass('adv-open'); setLink(open);
        // It opens downwards, often off screen: once it has finished growing, scroll just far enough to show all of it
        if (open) {
          var body = document.getElementById('link_body'), done = false;
          var show = function() {
            if (done) return; done = true; body.removeEventListener('transitionend', onEnd);
            var el = document.getElementById('sec_linking');
            if (el && el.scrollIntoView) el.scrollIntoView({block: 'nearest', behavior: window.matchMedia('(prefers-reduced-motion: reduce)').matches ? 'auto' : 'smooth'});
          };
          var onEnd = function(e) { if (e.target === body && e.propertyName === 'grid-template-rows') show(); };
          body.addEventListener('transitionend', onEnd);
          setTimeout(show, 700);      // in case no transition runs (reduced motion)
        }
      });

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
      // "Go to Linking assumptions" beside the linked check in the mortality a/b hint
      $(document).on('click', '.hint-goto', function(e) {
        e.preventDefault(); setLink(true); scrollToSection('sec_linking'); spy();
      });
      var spyQueued = false;
      function spy() {
        spyQueued = false;
        var sc = pageScroller(), base = sc ? sc.getBoundingClientRect().top : 0, active = null;
        // only sections that have a chip count (the author card, for one, has none)
        var secs = $('.assump-section:visible, .about-section:visible').filter(function() { return $('.jump-chip[data-target="' + this.id + '"]').length > 0; });
        // The section with the most of itself on screen wins (below the jump bar), so one that has mostly scrolled past stops counting
        var viewTop = base + 50, viewBottom = base + (sc ? sc.clientHeight : window.innerHeight), best = -1, bestIdx = 0, refIdx = 0, ids = [];
        var pos = sc ? sc.scrollTop : window.scrollY;
        var remaining = (sc ? sc.scrollHeight - sc.clientHeight : document.documentElement.scrollHeight - window.innerHeight) - pos;
        var viewH = viewBottom - viewTop;
        // In the last screenful the page runs out of room, so short sections near the end could never win on area and would be skipped.
        // There, a reference line slides from the top of the view to the bottom as the end is reached, and every section gets its turn in order.
        var refY = remaining < viewH ? viewTop + (1 - remaining / viewH) * viewH : -Infinity;
        secs.each(function(i) {
          var r = this.getBoundingClientRect(), shown = Math.min(r.bottom, viewBottom) - Math.max(r.top, viewTop);
          ids.push(this.id);
          if (shown > best) { best = shown; bestIdx = i; }
          if (r.top <= refY) refIdx = i;
        });
        if (ids.length) active = ids[Math.max(bestIdx, refIdx)];
        var atBottom = sc ? (sc.scrollTop + sc.clientHeight >= sc.scrollHeight - 2)
                          : (window.scrollY + window.innerHeight >= document.documentElement.scrollHeight - 2);
        if (atBottom && secs.length && sc && sc.scrollTop > 0) active = secs.last().attr('id');
        if (secs.length && (sc ? sc.scrollTop : window.scrollY) <= 2) active = secs.first().attr('id');   // at the very top the first section wins, however short it is
        if (forcedChip && Date.now() < forcedUntil && $('#' + forcedChip).is(':visible')) active = forcedChip;
        $('.jump-chip').removeClass('active').filter('[data-target="' + active + '"]').addClass('active');
      }
      document.addEventListener('scroll', function() { if (!spyQueued) { spyQueued = true; requestAnimationFrame(spy); } }, true);
      setTimeout(spy, 700);

      if (window.matchMedia('(prefers-reduced-motion: reduce)').matches) $.fx.off = true;
      // Getting started: dismissal is remembered (when the browser allows it) and leaves a one-line link
      function setHowto(show) {
        var el = document.getElementById('howto'), link = $('#howto_open'), L = link[0], cb = el.querySelector('.howto-close');
        try { if (show) localStorage.removeItem('vc_howto'); else localStorage.setItem('vc_howto', 'dismissed'); } catch (e) {}
        var reduce = window.matchMedia('(prefers-reduced-motion: reduce)').matches;
        if (!el.animate || reduce) { $(el).toggle(show); link.css('display', show ? 'none' : 'inline-block'); return; }
        // Measure both pieces at their full size first, then fold one away while the other opens in the same space
        el.getAnimations().concat(L.getAnimations()).forEach(function(x) { x.cancel(); });
        el.style.display = 'block'; el.style.overflow = 'hidden';
        L.style.display = 'block'; L.style.overflow = 'hidden';
        var cs = getComputedStyle(el), lcs = getComputedStyle(L);
        var full  = {height: el.offsetHeight + 'px', opacity: 1, marginBottom: cs.marginBottom, paddingTop: cs.paddingTop, paddingBottom: cs.paddingBottom,
                     borderTopWidth: cs.borderTopWidth, borderBottomWidth: cs.borderBottomWidth};
        var none  = {height: '0px', opacity: 0, marginBottom: '0px', paddingTop: '0px', paddingBottom: '0px', borderTopWidth: '0px', borderBottomWidth: '0px'};
        var lfull = {height: L.offsetHeight + 'px', opacity: 1, marginBottom: lcs.marginBottom};
        var lnone = {height: '0px', opacity: 0, marginBottom: '0px'};
        var opts = {duration: 420, easing: 'cubic-bezier(.4,0,.2,1)', fill: 'forwards'}, finished = false;
        if (cb) cb.style.opacity = 0;                       // the little arrow tab hangs outside the card, so it waits until the card is still
        function finish() {
          if (finished) return; finished = true;
          el.getAnimations().concat(L.getAnimations()).forEach(function(x) { x.cancel(); });
          el.style.overflow = ''; L.style.overflow = '';
          el.style.display = show ? '' : 'none'; L.style.display = show ? 'none' : 'block';
          if (cb) { cb.style.opacity = ''; if (show) cb.animate([{opacity: 0}, {opacity: 1}], {duration: 220}); }
        }
        var a1 = el.animate(show ? [none, full] : [full, none], opts);
        L.animate(show ? [lfull, lnone] : [lnone, lfull], opts);
        a1.onfinish = finish; setTimeout(finish, 650);
      }
      $(document).on('click', '.howto-close', function() { setHowto(false); });
      $(document).on('click', '#howto_open', function() { setHowto(true); });
      try { if (localStorage.getItem('vc_howto') === 'dismissed') { $('#howto').hide(); $('#howto_open').css('display', 'block'); } } catch (e) {}

      // Start over: drop any settings from the address, then reload the app
      Shiny.addCustomMessageHandler('startOver', function(msg) {
        try { var w = topWin(); w.history.replaceState(null, '', w.location.pathname + w.location.search); } catch (e) {}
        // Everything back to how the page looks on a first visit: the Getting started panel returns too
        try { localStorage.removeItem('vc_howto'); localStorage.removeItem('vc_ui'); } catch (e) {}
        // A fresh navigation, not a reload, so the browser cannot refill the form fields with the old values
        window.location.replace(window.location.href.split('#')[0]);
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
      // When the results are out of date the button keeps its blue, changes its label and glows amber around the edge
      // When the results go out of date the button crossfades to amber, the word "new" slides in, and one soft amber ring pulses outward.
      function syncRunStale() {
        var stale = $('#stale_note .stale-note').length > 0, b = $('#run');
        if (!b.length) return;
        $('body').toggleClass('results-stale', stale);   // also lights the dots beside the result tabs amber
        var was = b.hasClass('stale'), running = b.hasClass('running');
        b.toggleClass('stale', stale);
        if (!running) setRunLabel(stale ? 'Run new simulation' : 'Run simulation');
        if (stale && !was && !running && !PV_GATE.on && !window.matchMedia('(prefers-reduced-motion: reduce)').matches) {
          b.removeClass('ping'); void b[0].offsetWidth; b.addClass('ping');
          setTimeout(function () { b.removeClass('ping'); }, 1000);
        }
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
      // When the note appears, open it from zero height so the run status and Past runs slide down instead of jumping
      function growStaleNote(node) {
        var h = node.getBoundingClientRect().height;
        if (!node.animate || !h || window.matchMedia('(prefers-reduced-motion: reduce)').matches) return;
        node.style.overflow = 'hidden';
        var anim = node.animate([
          {height: '0px', opacity: 0, marginTop: '0px', paddingTop: '0px', paddingBottom: '0px'},
          {height: h + 'px', opacity: 1, marginTop: '8px', paddingTop: '5px', paddingBottom: '5px'}
        ], {duration: 300, easing: 'ease'});
        anim.onfinish = anim.oncancel = function() { node.style.overflow = ''; };
      }
      $(function() {
        var n = document.getElementById('stale_note');
        if (n) new MutationObserver(function(muts) {
          muts.forEach(function(m) {
            Array.prototype.forEach.call(m.addedNodes, function(node) {
              if (node.nodeType === 1 && node.classList.contains('stale-note')) growStaleNote(node);
            });
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
        if ($('#howto').is(':visible')) setHowto(false);
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
  

// Dim old results and spin the Run button while the server is working (only after a short delay, so quick updates do not flicker)
(function () {
  var t = null;
  $(document).on('shiny:busy', function () { clearTimeout(t); t = setTimeout(function () { document.body.classList.add('is-busy'); }, 250); });
  $(document).on('shiny:idle', function () { clearTimeout(t); document.body.classList.remove('is-busy'); });
})();

// Help bubbles: place the explanation beside the "?" (fixed, so the scrolling sidebar does not clip it)
(function () {
  function place(tip) {
    var t = tip.querySelector('.help-tip-text'); if (!t) return;
    var r = tip.getBoundingClientRect(), x = r.right + 12;
    if (x + 270 > window.innerWidth) x = Math.max(8, window.innerWidth - 270);
    t.style.left = x + 'px'; t.style.top = Math.max(8, Math.min(r.top - 8, window.innerHeight - t.offsetHeight - 8)) + 'px';
  }
  $(document).on('mouseenter focusin', '.help-tip', function () { place(this); });
})();

// Trials box: shows thousands separators, and its own arrows step through 500, 1,000, 5,000, 10,000 (any number can still be typed)
(function () {
  var ladder = [500, 1000, 5000, 10000];
  function digits(str) { return String(str).replace(/[^0-9]/g, ''); }
  function fmt(str) { var d = digits(str); return d === '' ? '' : Number(d).toLocaleString('en-US'); }
  function setVal(el, text) { el.value = text; $(el).trigger('input').trigger('change'); }
  function bump(el, dir) {
    var d = digits(el.value), v = d === '' ? NaN : Number(d), next;
    if (isNaN(v)) next = ladder[1];
    else if (dir > 0) next = v > ladder[ladder.length - 1] ? v : (ladder.filter(function (x) { return x > v; })[0] || v);
    else next = v < ladder[0] ? v : (ladder.filter(function (x) { return x < v; }).pop() || v);
    setVal(el, next.toLocaleString('en-US'));
  }
  // Same hover arrows on Trials (ladder steps) and Seed (steps of 1)
  function addStepper(el, onStep) {
    if (!el || el.dataset.stepper) return;
    el.dataset.stepper = '1';
    var wrap = document.createElement('div'); wrap.className = 'stepper-btns';
    wrap.innerHTML = '<button type="button" tabindex="-1" aria-label="Increase" class="up"></button><button type="button" tabindex="-1" aria-label="Decrease" class="down"></button>';
    el.parentNode.style.position = 'relative'; el.parentNode.appendChild(wrap);
    $(wrap).on('mousedown', function (e) { e.preventDefault(); });
    $(wrap).on('click', 'button', function () { onStep($(this).hasClass('up') ? 1 : -1); });
  }
  $(document).on('shiny:connected', function () {
    var el = document.getElementById('n_iter');
    if (el) { el.setAttribute('inputmode', 'numeric'); el.setAttribute('autocomplete', 'off'); addStepper(el, function (dir) { bump(el, dir); }); }
    var sd = document.getElementById('seed');
    if (sd) addStepper(sd, function (dir) { dir > 0 ? sd.stepUp() : sd.stepDown(); $(sd).trigger('input').trigger('change'); });
  });
  // Reformat as the user types, keeping the cursor next to the same digit
  $(document).on('input', '#n_iter', function (e) {
    var el = this, pos = el.selectionStart, before = el.value, digitsLeft = digits(before.slice(0, pos)).length, out = fmt(before);
    if (out === before) return;
    el.value = out;
    var n = 0, i = 0; while (i < out.length && n < digitsLeft) { if (/[0-9]/.test(out[i])) n++; i++; }
    try { el.setSelectionRange(i, i); } catch (err) {}
  });
  $(document).on('keydown', '#n_iter', function (e) {
    if (e.key === 'ArrowUp' || e.key === 'ArrowDown') { e.preventDefault(); bump(this, e.key === 'ArrowUp' ? 1 : -1); }
  });
})();

// Hover on the forecast and sensitivity charts. The server sends each bar's position (in the chart's own data units) and its tooltip
// text; the chart's coordinate map arrives with each drawing. Here the mouse position is turned into data units, the bar under it is
// found, and a highlight and tooltip are drawn over the picture, so they follow the mouse at once with no round trip to the server.
(function () {
  var bars = {}, maps = {};
  Shiny.addCustomMessageHandler('hoverBars', function (m) { bars[m.chart] = m.bars || []; $('.tip-cell').each(function () { hide(this); }); $(document).trigger('pv:bars', [m]); });
  $(document).on('shiny:value', function (e) {
    if (e.name === 'forecast_plot' || e.name === 'sens_plot') maps[e.name] = e.value && e.value.coordmap;
  });
  function parts(cell) {
    var hl = cell.querySelector('.bar-hl'), tip = cell.querySelector('.js-tip');
    if (!hl) { hl = document.createElement('div'); hl.className = 'bar-hl'; cell.appendChild(hl); }
    if (!tip) { tip = document.createElement('div'); tip.className = 'surv-tip js-tip'; cell.appendChild(tip); }
    return {hl: hl, tip: tip};
  }
  function linkTile(id) { $('.driver-tile').each(function () { $(this).toggleClass('linked', id != null && $(this).data('id') === id); }); }
  // A driver tile above the sensitivity chart lights up its bar, and (in mousemove below) a bar lights up its tile
  window.pvBarHl = function (chart, id) {
    var out = document.getElementById(chart), cell = out && out.closest('.tip-cell'), img = out && out.querySelector('img'); if (!cell) return;
    var list = bars[chart], map = maps[chart], panel = map && map.panels && map.panels[0], hit = null, i;
    if (id != null && list) for (i = 0; i < list.length; i++) if (list[i].id === id) { hit = list[i]; break; }
    if (!hit || !img || !panel) { var h0 = cell.querySelector('.bar-hl'), t0 = cell.querySelector('.js-tip'); if (h0) h0.classList.remove('on'); if (t0) t0.style.display = 'none'; cell.classList.remove('hl-linked'); return; }
    var r0 = cell.getBoundingClientRect(), ir = img.getBoundingClientRect(), d = panel.domain, g = panel.range, sx = map.dims.width / ir.width, sy = map.dims.height / ir.height;
    function cx(v) { return (g.left + (v - d.left) / (d.right - d.left) * (g.right - g.left)) / sx + ir.left - r0.left; }
    function cy(v) { return (g.bottom - (v - d.bottom) / (d.top - d.bottom) * (g.bottom - g.top)) / sy + ir.top - r0.top; }
    var p = parts(cell);
    p.hl.style.left = cx(hit.x0) + 'px'; p.hl.style.width = (cx(hit.x1) - cx(hit.x0)) + 'px';
    p.hl.style.top = cy(hit.y1) + 'px'; p.hl.style.height = (cy(hit.y0) - cy(hit.y1)) + 'px';
    p.hl.classList.add('on'); cell.classList.add('hl-linked');
    cell.setAttribute('data-chart', chart);
    if (p.tip.dataset.html !== hit.tip) { p.tip.innerHTML = hit.tip; p.tip.dataset.html = hit.tip; }
    p.tip.style.display = 'block';
  };
  $(document).on('mouseenter focusin', '.driver-tile', function () { window.pvBarHl('sens_plot', $(this).data('id')); });
  $(document).on('mouseleave focusout', '.driver-tile', function () { window.pvBarHl('sens_plot', null); });
  function hide(cell) {
    linkTile(null);
    var hl = cell.querySelector('.bar-hl'), tip = cell.querySelector('.js-tip');
    if (hl) hl.classList.remove('on'); if (tip) tip.style.display = 'none';
  }
  $(document).on('mousemove', '.tip-cell', function (e) {
    var cell = this, out = cell.querySelector('.shiny-plot-output'), img = out && out.querySelector('img');
    if (out && cell.getAttribute('data-chart') !== out.id) cell.setAttribute('data-chart', out.id);
    var r0 = cell.getBoundingClientRect(), x = e.clientX - r0.left;
    cell.style.setProperty('--tx', x + 'px'); cell.style.setProperty('--ty', (e.clientY - r0.top) + 'px');
    cell.classList.toggle('tip-flip', x > r0.width / 2);
    var list = out && bars[out.id], map = out && maps[out.id], panel = map && map.panels && map.panels[0];
    if (!img || !list || !panel || e.buttons) return hide(cell);   // nothing while dragging a range
    var ir = img.getBoundingClientRect(), sx = map.dims.width / ir.width, sy = map.dims.height / ir.height;
    var d = panel.domain, g = panel.range, px = (e.clientX - ir.left) * sx, py = (e.clientY - ir.top) * sy;
    var dx = d.left + (px - g.left) / (g.right - g.left) * (d.right - d.left);
    var dy = d.bottom + (g.bottom - py) / (g.bottom - g.top) * (d.top - d.bottom);
    var hit = null;
    for (var i = 0; i < list.length; i++) { var b = list[i]; if (dx >= b.x0 && dx <= b.x1 && dy >= b.y0 && dy <= b.y1) { hit = b; break; } }
    if (!hit) return hide(cell);
    function cx(v) { return (g.left + (v - d.left) / (d.right - d.left) * (g.right - g.left)) / sx + ir.left - r0.left; }
    function cy(v) { return (g.bottom - (v - d.bottom) / (d.top - d.bottom) * (g.bottom - g.top)) / sy + ir.top - r0.top; }
    var p = parts(cell);
    p.hl.style.left = cx(hit.x0) + 'px'; p.hl.style.width = (cx(hit.x1) - cx(hit.x0)) + 'px';
    p.hl.style.top = cy(hit.y1) + 'px'; p.hl.style.height = (cy(hit.y0) - cy(hit.y1)) + 'px';
    p.hl.classList.add('on'); linkTile(hit.id);
    if (p.tip.dataset.html !== hit.tip) { p.tip.innerHTML = hit.tip; p.tip.dataset.html = hit.tip; }
    p.tip.style.display = 'block';
  });
  $(document).on('mouseleave', '.tip-cell', function () { hide(this); });
})();

// Survival charts: tell the app when the mouse leaves a chart so the highlighted line is cleared
$(document).on('mouseleave', '.surv-cell', function () { Shiny.setInputValue('surv_leave', Date.now(), {priority: 'event'}); });

// The light bulb in "What this says" glows yellow for a few seconds, but only the first time a tab is opened after new results
// (the tabs with the purple light). If the text arrives after the tab opens, the glow starts when it does.
(function () {
  var timer = null, pending = null;
  function activeTab() { return $('#tabs li.active a').data('value'); }
  function tryGlow() {
    if (!pending || pending !== activeTab()) return;
    var ic = $('.tab-pane.active .insight-icon');
    if (!ic.length) return;
    pending = null;
    ic.removeClass('glow'); void ic[0].offsetWidth; ic.addClass('glow');
    clearTimeout(timer); timer = setTimeout(function() { ic.removeClass('glow'); }, 3800);
  }
  // "show" fires before the purple light is cleared, so it tells us whether this is a first visit
  $(document).on('show.bs.tab', '#tabs a[data-toggle="tab"]', function() {
    if ($(this).hasClass('tab-new')) pending = $(this).data('value');
  });
  $(document).on('shown.bs.tab', '#tabs a[data-toggle="tab"]', function() { setTimeout(tryGlow, 40); });
  $(document).on('shiny:value', function(e) { if (/^insight_/.test(e.name)) setTimeout(tryGlow, 150); });
})();

// A short guided tour: spotlights the main parts of the page one at a time
(function () {
  var steps = [
    {tab: 'Define assumptions', sel: '.well', side: 'right', title: 'Simulation settings',
     text: 'Choose the mortality model and age structure, the number of trials and a seed, and a preset. Each assumption is drawn from a distribution, so the answer comes out as a spread, not a single number.'},
    {tab: 'Define assumptions', sel: '#sec_transmission .cards-grid', side: 'bottom', title: 'Assumptions',
     text: 'Each card is one assumption. Click a card to change its distribution or its values. The defaults come from the literature.'},
    {tab: 'Define assumptions', sel: '.run-dock', side: 'right', title: 'Run the simulation',
     text: 'Press Run simulation (or Cmd/Ctrl + Enter) after you change anything. The app already ran once when the page opened, so results are waiting. Press Next to open them.'},
    {tab: 'Forecast', sel: '.tab-pane.active .insight', side: 'bottom', wait: 1600, title: 'A plain-language reading',
     text: 'Every results tab starts with a short, automatically generated summary of what the numbers mean. Click the box to read it. The bulb glows the first time you open a tab after new results.'},
    {tab: 'Forecast', sel: '#tabs', side: 'bottom', title: 'The results tabs',
     text: 'Forecast shows the spread of Ct. Sensitivity shows which assumptions matter most. Assumption draws shows the values drawn, and Survival curves shows how long mosquitoes live. Model check and About hold the checks, equations and how to cite.',
     note: 'A purple dot on a tab means it has new results you have not looked at yet. An amber dot means your settings have changed since that run.'}
  ];
  var i = 0, spot = null, pop = null, active = false;

  function showTab(name) { $('#tabs a[data-value="' + name + '"]').tab('show'); }
  function end() {
    active = false; if (spot) spot.remove(); if (pop) pop.remove(); spot = pop = null;
    $(document).off('.tour'); $(window).off('.tour');
    try { localStorage.setItem('vc_tour', 'done'); } catch (e) {}
  }
  function place() {
    if (!active) return;
    var st = steps[i], el = $(st.sel).filter(':visible')[0];
    if (!el) { spot.css({opacity: 0}); return; }
    var r = el.getBoundingClientRect(), pad = 6;
    spot.css({opacity: 1, left: r.left - pad, top: r.top - pad, width: r.width + 2 * pad, height: r.height + 2 * pad});
    var pw = pop.outerWidth(), ph = pop.outerHeight(), vw = window.innerWidth, vh = window.innerHeight, x, y;
    if (st.side === 'right' && r.right + pw + 24 < vw) { x = r.right + 16; y = Math.max(12, Math.min(r.top, vh - ph - 12)); }
    else { x = Math.max(12, Math.min(r.left, vw - pw - 12)); y = r.bottom + 16;
           if (y + ph > vh - 12) y = Math.max(12, r.top - ph - 16); }
    pop.css({left: x, top: y});
  }
  function render() {
    var st = steps[i];
    pop.html('<div class="tour-step">Step ' + (i + 1) + ' of ' + steps.length + '</div><div class="tour-title"></div><p class="tour-text"></p>' +
             (st.note ? '<p class="tour-note"><span class="legend-dot"></span><span class="tour-note-text"></span></p>' : '') +
             '<div class="tour-btns"><button type="button" class="tour-skip">Skip</button><span class="tour-gap"></span>' +
             (i > 0 ? '<button type="button" class="tour-back">Back</button>' : '') +
             '<button type="button" class="tour-next">' + (i === steps.length - 1 ? 'Done' : 'Next') + '</button></div>');
    pop.find('.tour-title').text(st.title); pop.find('.tour-text').text(st.text); if (st.note) pop.find('.tour-note-text').text(st.note);
    var el = $(st.sel).filter(':visible')[0];
    if (el && el.scrollIntoView) el.scrollIntoView({block: 'nearest'});
    place(); setTimeout(place, 350);
  }
  function go(n) {
    if (n < 0) return;
    if (n >= steps.length) { end(); return; }
    i = n; var st = steps[i];
    if (st.tab && $('#tabs li.active a').data('value') !== st.tab) showTab(st.tab);
    setTimeout(render, st.wait || (st.tab ? 450 : 50));
  }
  function start() {
    if (active) return;
    active = true; i = 0;
    spot = $('<div class="tour-spot" aria-hidden="true">').appendTo('body');
    pop = $('<div class="tour-pop" role="dialog" aria-label="Guided tour">').appendTo('body');
    $(document).on('click.tour', '.tour-next', function() { go(i + 1); })
               .on('click.tour', '.tour-back', function() { go(i - 1); })
               .on('click.tour', '.tour-skip', end)
               .on('keydown.tour', function(e) { if (e.key === 'Escape') end(); else if (e.key === 'ArrowRight') go(i + 1); else if (e.key === 'ArrowLeft') go(i - 1); });
    $(window).on('resize.tour', place); document.addEventListener('scroll', place, true);
    go(0);
  }
  $(document).on('click', '#tour_start', start);
})();

// "What this says" is a closed box until it is clicked; the choice stays while the text is refreshed
$(document).on('click', '.insight-head', function() {
  var box = $(this).closest('.shiny-html-output').toggleClass('insight-open');
  $(this).attr('aria-expanded', box.hasClass('insight-open') ? 'true' : 'false');
});
$(document).on('shiny:value', function(e) {
  if (!/^insight_/.test(e.name)) return;
  setTimeout(function() {
    var box = $('#' + e.name); box.find('.insight-head').attr('aria-expanded', box.hasClass('insight-open') ? 'true' : 'false');
  }, 60);
});

// Past runs: click a run to reload its settings, the pencil renames it, and the earlier runs fold away
(function () {
  // Clicking a run fades that run's card and lays a short message over it, so nothing above it moves. The message keeps following its
  // card for as long as it shows: when the run finishes, the card gains tags and the list redraws, and the message resizes with it.
  function flash(item) {
    var box = $('.history-box').first(); if (!box.length) return;
    box.find('.scen-flash').remove();
    var id = $(item).data('id');
    var msg = $('<div class="scen-flash" role="status"><svg class="scen-check" viewBox="0 0 24 24" aria-hidden="true"><circle cx="12" cy="12" r="10"/><path d="M7 12.5l3.2 3.2L17 8.8"/></svg><span>Settings loaded. Click Run to use them.</span></div>').appendTo(box);
    var gone = false;
    function place() {
      if (gone) return;
      var card = box.find('.scen-item').filter(function () { return $(this).data('id') === id; })[0];
      if (card) {
        var br = box[0].getBoundingClientRect(), ir = card.getBoundingClientRect();
        if (ir.height > 0) msg.css({top: ir.top - br.top - box[0].clientTop + box[0].scrollTop, left: ir.left - br.left - box[0].clientLeft, width: ir.width, height: ir.height, visibility: ''});
        else msg.css('visibility', 'hidden');   // the card is folded away (for example under "earlier runs")
      }
    }
    (function tick() { place(); if (!gone) requestAnimationFrame(tick); })();
    var timer = setInterval(place, 120);   // keeps following even when frames are not being drawn
    requestAnimationFrame(function () { msg.addClass('on'); });
    setTimeout(function () {
      msg.removeClass('on');
      setTimeout(function () { gone = true; clearInterval(timer); msg.remove(); }, 450);
    }, 2200);
  }
  function load(item) { flash(item); Shiny.setInputValue('scenario_load', Number($(item).data('id')), {priority: 'event'}); }
  $(document).on('click', '.scen-item', function(e) {
    if ($(e.target).closest('.scen-edit, .scen-cmp, .scen-run, .scen-del, .scen-dl, .scen-input').length) return;
    load(this);
  });
  $(document).on('keydown', '.scen-item', function(e) {
    if ($(e.target).is('input')) return;
    if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); load(this); }
  });
  $(document).on('click', '.scen-edit', function(e) {
    e.stopPropagation();
    var item = $(this).closest('.scen-item'), nm = item.find('.scen-name');
    if (item.find('.scen-input').length) return;
    var inp = $('<input type="text" class="scen-input" maxlength="40" aria-label="Run name">').val(nm.text());
    nm.hide().after(inp); inp.focus().select();
    var done = false;
    function finish(save) {
      if (done) return; done = true;
      var val = $.trim(inp.val());
      inp.remove(); nm.show();
      if (save && val !== nm.text()) Shiny.setInputValue('scenario_rename', {id: Number(item.data('id')), name: val, t: Date.now()}, {priority: 'event'});
    }
    inp.on('keydown', function(ev) { ev.stopPropagation(); if (ev.key === 'Enter') finish(true); else if (ev.key === 'Escape') finish(false); });
    inp.on('blur', function() { finish(true); });
    inp.on('click', function(ev) { ev.stopPropagation(); });
  });
  $(document).on('click', '.scen-more', function() {
    var box = $(this).closest('.shiny-html-output').toggleClass('scen-expanded');
    $(this).attr('aria-expanded', box.hasClass('scen-expanded') ? 'true' : 'false');
  });
})();

// Past runs starts rolled up; clicking its header slides it open or shut
$(document).on('click', '.history-head', function() {
  var box = $(this).closest('.history-box').toggleClass('open');
  $(this).attr('aria-expanded', box.hasClass('open') ? 'true' : 'false');
});

// Compare buttons on past runs, and opening the Compare runs section when asked
$(document).on('click', '.scen-cmp', function(e) {
  e.stopPropagation();
  Shiny.setInputValue('scenario_compare', Number($(this).closest('.scen-item').data('id')), {priority: 'event'});
});
function openTool(id) {
  $('.tool-panel').removeClass('on'); $('.tool-tab').removeClass('on').attr('aria-expanded', 'false');
  $('#tool_' + id).addClass('on'); $('.tool-tab[data-panel="' + id + '"]').addClass('on').attr('aria-expanded', 'true');
}
$(function() {
  // The compare icon on a past run: go to the Forecast tab, scroll down to Dig deeper, then slide the compare panel open
  Shiny.addCustomMessageHandler('openCompare', function(msg) {
    if ($('#tabs li.active a').data('value') !== 'Forecast') $('#tabs a[data-value="Forecast"]').tab('show');
    setTimeout(function() {
      var t = $('.tools')[0]; if (t) t.scrollIntoView({behavior: 'smooth', block: 'start'});
      setTimeout(function() {
        openTool('compare');
        setTimeout(function() { var p = $('#tool_compare')[0]; if (p) p.scrollIntoView({behavior: 'smooth', block: 'nearest'}); }, 480);
      }, 500);
    }, 450);
  });
});
// "Dig deeper": one panel open at a time; clicking the open one closes it
$(document).on('click', '.tool-tab', function() {
  var id = $(this).data('panel');
  if ($('#tool_' + id).hasClass('on')) { $('.tool-panel').removeClass('on'); $('.tool-tab').removeClass('on').attr('aria-expanded', 'false'); }
  else openTool(id);
});

// Past runs: reload and run, remove one, clear all
$(document).on('click', '.scen-run', function(e) {
  e.stopPropagation();
  Shiny.setInputValue('scenario_run', Number($(this).closest('.scen-item').data('id')), {priority: 'event'});
});
$(document).on('click', '.scen-del', function(e) {
  e.stopPropagation();
  Shiny.setInputValue('scenario_del', Number($(this).closest('.scen-item').data('id')), {priority: 'event'});
});
$(document).on('click', '.scen-clear', function() { Shiny.setInputValue('scenario_clear', Date.now(), {priority: 'event'}); });
$(function() {
  // After a past run's settings are loaded the controls take a moment to settle; press Run once they have been quiet for a bit
  Shiny.addCustomMessageHandler('runSoon', function(msg) {
    var start = Date.now(), last = Date.now();
    $(document).on('shiny:inputchanged.runsoon', function() { last = Date.now(); });
    var timer = setInterval(function() {
      var quiet = Date.now() - last > 1000 && !$('html').hasClass('shiny-busy');
      if (quiet || Date.now() - start > 9000) {
        clearInterval(timer); $(document).off('.runsoon');
        if (quiet) $('#run').trigger('click');
      }
    }, 150);
  });
});


// Links that open the About tab at a given section
$(function() {
  Shiny.addCustomMessageHandler('gotoAbout', function(msg) {
    if ($('#tabs li.active a').data('value') !== 'About') $('#tabs a[data-value="About"]').tab('show');
    setTimeout(function() {
      var el = document.getElementById(msg.target); if (!el) return;
      el.scrollIntoView({behavior: 'smooth', block: 'start'});
      $(el).addClass('card-flash'); setTimeout(function() { $(el).removeClass('card-flash'); }, 2200);
    }, 500);
  });
});

// Temperature section (and its "Baseline and settings" fold): a plain <details> snaps open and shut, which makes everything
// below it jump. Animate the height instead so the rest of the sidebar slides out of the way.
(function () {
  var running = new WeakMap(), target = new WeakMap();   // the animation in progress, and whether it ends open
  $(document).on('click', '.temp-section > summary, .temp-sens > summary', function (e) {
    if ($(e.target).closest('.temp-switch').length) return;   // the on/off switch is not part of the fold's toggle
    var d = this.parentNode;
    if (!d.animate || window.matchMedia('(prefers-reduced-motion: reduce)').matches) return;   // fall back to the normal toggle
    e.preventDefault();
    var start = d.getBoundingClientRect().height, prev = running.get(d);
    var closing = prev ? target.get(d) : d.open;            // a click during an animation turns it around
    if (prev) prev.cancel();
    d.style.overflow = 'hidden';
    var end;
    if (closing) {
      var cs = getComputedStyle(d);
      end = this.getBoundingClientRect().height + (parseFloat(cs.borderTopWidth) || 0) + (parseFloat(cs.borderBottomWidth) || 0);
    } else { d.open = true; end = d.scrollHeight; }
    var anim = d.animate([{height: start + 'px'}, {height: end + 'px'}], {duration: 300, easing: 'cubic-bezier(.4,0,.2,1)'});
    running.set(d, anim); target.set(d, !closing);
    anim.onfinish = function () { d.open = !closing; d.style.overflow = ''; running.delete(d); };
    anim.oncancel = function () { d.style.overflow = ''; };
  });
})();


// Temperature slider: a thin track with a fill that grows out from zero (blue for cooler, orange for warmer), a round handle, a readout,
// and quick-pick chips. The value lives in a hidden number box (#temp_delta) that Shiny reads, so saving, loading and presets work as
// for any input. Clicking the track glides the handle to that point; dragging follows the pointer directly. The handle, the fill and
// the colour are all drawn from one number (--p), so they cannot drift apart.
(function () {
  var MIN = -8, MAX = 8, STEP = 0.1, target = 0, shown = 0, anim = null, selfChange = false, commitTimer = null, prevTarget = null;
  function box() { return $('.temp-slider'); }
  function round1(v) { return Math.round(v * 10) / 10; }
  function clamp(v) { return round1(Math.min(MAX, Math.max(MIN, Math.round(v / STEP) * STEP))); }
  // The badge in the section header shows while the switch is on and a change is set
  function badge(v) { var cb = document.getElementById('temp_on'); $('.temp-section').toggleClass('badge-on', !!(cb && cb.checked && v !== 0)); }
  window.tempBadge = function () { badge(target); };
  function fmt(v) { return v === 0 ? '0' : (v > 0 ? '+' : '\u2212') + Math.abs(v); }
  function paint(v) { shown = v; var b = box()[0]; if (b) b.style.setProperty('--p', (v - MIN) / (MAX - MIN)); }
  function ui(v) {
    var b = box(); if (!b.length) return;
    b.toggleClass('is-warm', v > 0).toggleClass('is-cool', v < 0).toggleClass('is-zero', v === 0);
    var txt = v === 0 ? 'No change' : (v > 0 ? '+' : '\u2212') + Math.abs(v) + ' \u00b0C';
    var field = document.getElementById('temp_value'); if (field && document.activeElement !== field) field.value = fmt(v);
    b.find('.ts-handle').attr({'aria-valuenow': v, 'aria-valuetext': txt});
    b.find('.temp-chip').each(function () { $(this).toggleClass('on', Number($(this).data('v')) === v); });
    if (v !== 0) $('#temp_badge').text((v > 0 ? '+' : '\u2212') + Math.abs(v) + ' \u00b0C');   // keeps its text while it fades out at zero
    badge(v);
    $('.temp-section').toggleClass('has-temp', v !== 0);     // the on/off switch only shows once a change is set
    // A move to a non-zero value switches the section on, however it was made (pointer, keys, chips, a preset or loaded settings)
    if (prevTarget !== null && v !== prevTarget && v !== 0 && window.tempSetOn) window.tempSetOn(true);
    prevTarget = v;
  }
  // Tell Shiny. While dragging this is spread out, so the server is not flooded.
  function commit(now) {
    clearTimeout(commitTimer);
    var send = function () {
      var inp = document.getElementById('temp_delta'); if (!inp) return;
      inp.value = String(round1(target)); selfChange = true; $(inp).trigger('change'); selfChange = false;
    };
    if (now) send(); else commitTimer = setTimeout(send, 140);
  }
  function stopAnim() { if (anim) { cancelAnimationFrame(anim.id); anim = null; } }
  // Move to v: glide (ease-out over 260 ms) or jump straight there
  function setValue(v, how) {
    v = clamp(v); stopAnim(); target = v; ui(v);
    var reduce = window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;
    if (how === 'glide' && !reduce && shown !== v && window.requestAnimationFrame) {
      var from = shown, t0 = null, token = {};
      anim = {token: token};
      var step = function (ts) {
        if (!anim || anim.token !== token) return;
        if (t0 === null) t0 = ts;
        var p = Math.min(1, (ts - t0) / 260), e = 1 - Math.pow(1 - p, 3);
        paint(from + (v - from) * e);
        if (p < 1) anim.id = requestAnimationFrame(step); else { anim = null; paint(v); }
      };
      anim.id = requestAnimationFrame(step);
      setTimeout(function () { if (anim && anim.token === token) { anim = null; paint(v); } }, 500);   // frames not being drawn (a background tab)
    } else paint(v);
    if (how !== 'external') commit(how === 'glide' || how === 'end');
  }
  function valueAt(clientX, offset) {
    var tr = box().find('.ts-track')[0].getBoundingClientRect();
    return MIN + Math.min(1, Math.max(0, (clientX + (offset || 0) - tr.left) / tr.width)) * (MAX - MIN);
  }
  // Pointer: a click glides to the point; once the pointer moves it drags directly
  $(document).on('pointerdown', '.tslider', function (e) {
    if (e.button !== undefined && e.button !== 0) return;
    e.preventDefault();
    var slider = this, onHandle = $(e.target).closest('.ts-handle').length > 0;
    var hr = $(slider).find('.ts-handle')[0].getBoundingClientRect();
    var offset = onHandle ? (hr.left + hr.width / 2) - e.clientX : 0;   // grabbing the handle does not make it jump to the pointer
    var startX = e.clientX, moved = false;
    try { slider.setPointerCapture(e.pointerId); } catch (err) {}
    $(slider).addClass('dragging');
    if (!onHandle) setValue(valueAt(e.clientX), 'glide');
    function move(ev) {
      if (!moved && Math.abs(ev.clientX - startX) < 3) return;
      moved = true; setValue(valueAt(ev.clientX, offset), 'drag');
    }
    function up() {
      $(slider).off('pointermove.ts pointerup.ts pointercancel.ts').removeClass('dragging');
      if (moved) commit(true);
    }
    $(slider).on('pointermove.ts', move).on('pointerup.ts pointercancel.ts', up);
  });
  // Keyboard: arrows 0.5, with Shift 0.1; Page keys 2; Home and End go to the ends
  $(document).on('keydown', '.ts-handle', function (e) {
    var k = e.key, d = null, big = e.shiftKey ? 0.1 : 0.5;
    if (k === 'ArrowRight' || k === 'ArrowUp') d = big; else if (k === 'ArrowLeft' || k === 'ArrowDown') d = -big;
    else if (k === 'PageUp') d = 2; else if (k === 'PageDown') d = -2;
    else if (k === 'Home') { e.preventDefault(); return setValue(MIN, 'glide'); }
    else if (k === 'End') { e.preventDefault(); return setValue(MAX, 'glide'); }
    if (d === null) return;
    e.preventDefault(); setValue(target + d, 'drag');
  });
  $(document).on('click', '.temp-chip', function () { setValue(Number($(this).data('v')), 'glide'); });
  // Typing a value: Enter or leaving the box applies it (kept within -8 to 8, to 0.1); Escape puts the old value back; arrows nudge it
  function parseTemp(str) {
    var v = parseFloat(String(str).replace(/\u2212|\u2013/g, '-').replace(',', '.').replace(/[^0-9.+\-]/g, ''));
    return isNaN(v) ? null : v;
  }
  function applyTyped(inp) { var v = parseTemp(inp.value); if (v === null) { inp.value = fmt(target); return; } setValue(v, 'glide'); inp.value = fmt(target); }
  $(document).on('keydown', '#temp_value', function (e) {
    if (e.key === 'Enter') { e.preventDefault(); applyTyped(this); this.select(); }
    else if (e.key === 'Escape') { this.value = fmt(target); this.blur(); }
    else if (e.key === 'ArrowUp' || e.key === 'ArrowDown') {
      e.preventDefault(); var base = parseTemp(this.value); if (base === null) base = target;
      setValue(base + (e.key === 'ArrowUp' ? 1 : -1) * (e.shiftKey ? 1 : 0.1), 'drag'); this.value = fmt(target);
    }
  });
  $(document).on('blur', '#temp_value', function () { applyTyped(this); });
  $(document).on('focus', '#temp_value', function () { var el = this; setTimeout(function () { el.select(); }, 0); });
  // The server (a preset, Reset, loaded settings or a link) changed the number box: follow it
  $(document).on('change', '#temp_delta', function () {
    if (selfChange) return;
    var v = parseFloat(this.value); setValue(isNaN(v) ? 0 : v, 'external');
  });
  $(function () {
    var inp = document.getElementById('temp_delta'), v = inp ? parseFloat(inp.value) : 0;
    target = clamp(isNaN(v) ? 0 : v); paint(target); ui(target);
  });
})();


// Temperature on/off switch. The hidden checkbox #temp_on holds the state; the button flips it. Touching any temperature control
// switches it on, so a change is never made while the section is off by accident.
(function () {
  function setOn(on) {
    var cb = document.getElementById('temp_on'); if (!cb || cb.checked === on) return;
    cb.checked = on; $(cb).trigger('change');
  }
  function mirror() {
    var cb = document.getElementById('temp_on'); if (!cb) return;
    $('.temp-switch').attr('aria-checked', cb.checked ? 'true' : 'false');
    if (window.tempBadge) window.tempBadge();
  }
  $(document).on('click', '.temp-switch', function (e) {
    e.preventDefault(); e.stopPropagation();
    var cb = document.getElementById('temp_on'); setOn(!cb.checked); mirror();
  });
  window.tempSetOn = function (on) { setOn(on); mirror(); };
  $(document).on('change', '#temp_on', mirror);
  $(document).on('shiny:updateinput', function () { setTimeout(mirror, 0); });
  $(document).on('mousedown touchstart keydown change', '.temp-body', function (e) {
    if (e.type === 'change' && !e.originalEvent) return;   // changes made by code (presets, loaded settings) do not count
    setOn(true); mirror();
  });
  $(mirror);
})();

// Smooth resizing: when the content of a .smooth-h box is replaced or swapped (the temperature description when another curve is
// chosen, the fields under "Baseline and settings"), the box grows or shrinks to its new height instead of snapping, so everything
// below slides along with it.
(function () {
  function track(el) {
    var prev = el.getBoundingClientRect().height, anim = null;
    // Keep the last settled height up to date, including while the box is hidden or opened by something else
    new ResizeObserver(function () { if (!anim) prev = el.getBoundingClientRect().height; }).observe(el);
    new MutationObserver(function () {
      if (!el.animate || window.matchMedia('(prefers-reduced-motion: reduce)').matches) return;
      var start = anim ? el.getBoundingClientRect().height : prev;   // a change during an animation continues from where it is
      if (anim) { anim.cancel(); anim = null; }
      var end = el.getBoundingClientRect().height;                   // the new natural height
      if (!start && !end || Math.abs(start - end) < 1) return;
      if (!el.offsetParent && !end) return;
      anim = el.animate([{height: start + 'px'}, {height: end + 'px'}], {duration: 300, easing: 'cubic-bezier(.4,0,.2,1)'});
      var mine = anim;
      mine.onfinish = mine.oncancel = function () { if (anim === mine) { anim = null; prev = el.getBoundingClientRect().height; } };
    }).observe(el, {childList: true, subtree: true, characterData: true, attributes: true, attributeFilter: ['style', 'class']});
  }
  $(function () { $('.smooth-h').each(function () { track(this); }); });
})();


// Drag boxes (brushes) on the forecast and survival charts: remove the box when the app asks, even if Shiny's own reset missed it
$(function () {
  Shiny.addCustomMessageHandler('clearBrushes', function (plotIds) {
    [].concat(plotIds).forEach(function (id) { $('#' + id + '_brush').remove(); });
  });
});


// Toast: a short message with one action (Undo), fixed near the bottom of the window so it never pushes the page around.
(function () {
  var timer = null;
  function hide() { $('.pv-toast').removeClass('on'); clearTimeout(timer); }
  Shiny.addCustomMessageHandler('toast', function (m) {
    var t = $('.pv-toast');
    if (!t.length) t = $('<div class="pv-toast" role="status" aria-live="polite"><span class="pv-toast-text"></span>' +
                         '<button type="button" class="pv-toast-btn"></button>' +
                         '<button type="button" class="pv-toast-x" aria-label="Dismiss">&times;</button><span class="pv-toast-bar"></span></div>').appendTo('body');
    t.removeClass('err warn').addClass(m.kind === 'error' ? 'err' : (m.kind === 'warn' ? 'warn' : ''));
    t.find('.pv-toast-text').text(m.text);
    t.find('.pv-toast-btn').text(m.action || '').toggle(!!m.action).data('input', m.input || '');
    t.find('.pv-toast-bar').css('animation-duration', (m.ms || 8000) + 'ms');
    t.removeClass('on'); void t[0].offsetWidth;                  // restart the entrance and the countdown bar
    t.addClass('on'); clearTimeout(timer); timer = setTimeout(hide, m.ms || 8000);
  });
  $(document).on('click', '.pv-toast-x', hide);
  $(document).on('click', '.pv-toast-btn', function () {
    var id = $(this).data('input'); if (id) Shiny.setInputValue(id, Date.now(), {priority: 'event'});
    hide();
  });
  $(document).on('keydown', function (e) { if (e.key === 'Escape') hide(); });
})();


// Remember my place: the tab I was on, how far I had scrolled on each tab, and which parts of the page I had opened (assumption cards,
// Temperature, Past runs, the linking section). It is kept in this browser only and applied when the page is opened again; Start over
// clears it. Scroll positions are also kept per tab while using the app, so coming back to a tab puts you where you left it.
(function () {
  var KEY = 'vc_ui', st = {tab: null, scroll: {}, open: []}, userMoved = false, restoring = true;
  try { var saved = JSON.parse(localStorage.getItem(KEY) || 'null'); if (saved && typeof saved === 'object') st = $.extend(st, saved); } catch (e) {}
  function scroller() {
    var tc = $('.tabbable > .tab-content')[0];
    return tc && getComputedStyle(tc).overflowY !== 'visible' && tc.scrollHeight > tc.clientHeight + 1 ? tc : null;
  }
  function getY() { var s = scroller(); return s ? s.scrollTop : (window.pageYOffset || 0); }
  function setY(y) { var s = scroller(); if (s) s.scrollTop = y; else window.scrollTo(0, y); }
  function openIds() {
    var ids = [];
    $('.assump-card.open').each(function () { if (this.id) ids.push('#' + this.id); });
    $('.temp-section[open]').each(function () { ids.push('.temp-section'); });
    $('.temp-sens[open]').each(function () { ids.push('.temp-sens'); });
    $('.history-box.open').each(function () { ids.push('.history-box'); });
    $('.assump-section.adv-open').each(function () { if (this.id) ids.push('#' + this.id); });
    return ids;
  }
  var timer = null;
  function save() {
    clearTimeout(timer);
    timer = setTimeout(function () {
      if (restoring) return;
      var tab = $('#tabs li.active a').data('value'); if (tab) { st.tab = tab; st.scroll[tab] = getY(); }
      st.open = openIds();
      try { localStorage.setItem(KEY, JSON.stringify(st)); } catch (e) {}
    }, 250);
  }
  function applyOpen() {
    (st.open || []).forEach(function (sel) {
      var el = $(sel).first(); if (!el.length) return;
      if (el.is('details')) el.prop('open', true);
      else if (el.hasClass('assump-card')) el.addClass('open').find('.assump-toggle').attr('aria-expanded', 'true');
      else if (el.hasClass('history-box')) { el.addClass('open').find('.history-head').attr('aria-expanded', 'true'); }
      else if (el.hasClass('assump-section')) el.addClass('adv-open');
    });
  }
  // Put the scroll back once the tab has content tall enough to scroll to; give up if the user scrolls first
  function restoreScroll(y) {
    if (!y) return;
    var tries = 0, t = setInterval(function () {
      if (userMoved || ++tries > 40) { clearInterval(t); return; }
      setY(y);
      if (Math.abs(getY() - y) < 2) clearInterval(t);
    }, 400);
  }
  $(document).on('wheel touchstart keydown mousedown', function () { userMoved = true; });
  $(document).on('show.bs.tab', 'a[data-toggle="tab"]', function (e) {
    var prev = e.relatedTarget && $(e.relatedTarget).data('value');
    if (prev && !restoring) st.scroll[prev] = getY();
  });
  $(document).on('shown.bs.tab', 'a[data-toggle="tab"]', function () {
    if (restoring) return;
    var tab = $(this).data('value'); setY(st.scroll[tab] || 0); save();
  });
  document.addEventListener('scroll', function (e) { if (e.target.classList && e.target.classList.contains('tab-content')) save(); }, true);   // scroll does not bubble
  $(window).on('scroll', save);
  $(document).on('click change', save);
  $(window).on('beforeunload pagehide', function () { restoring = false; clearTimeout(timer); var tab = $('#tabs li.active a').data('value'); if (tab) { st.tab = tab; st.scroll[tab] = getY(); } st.open = openIds(); try { localStorage.setItem(KEY, JSON.stringify(st)); } catch (e) {} });
  // On load: reopen what was open, go back to the last tab, then to where I was on it once its results are drawn
  $(document).on('shiny:connected', function () {
    setTimeout(function () {
      applyOpen();
      var link = st.tab && $('#tabs a[data-value="' + st.tab + '"]');
      var go = function () {
        restoring = false;
        if (st.tab) restoreScroll(st.scroll[st.tab] || 0);
        save();
      };
      if (link && link.length && !link.parent().hasClass('active')) { link.tab('show'); setTimeout(go, 300); } else go();
    }, 400);
  });
})();


// Saving past runs: the app builds the file and sends its text here; this saves it through the browser.
$(function () {
  Shiny.addCustomMessageHandler('saveFile', function (m) {
    var blob = new Blob([m.text], {type: (m.mime || 'text/plain') + ';charset=utf-8'}), url = URL.createObjectURL(blob);
    var a = document.createElement('a'); a.href = url; a.download = m.name; document.body.appendChild(a); a.click();
    setTimeout(function () { URL.revokeObjectURL(url); a.remove(); }, 1000);
  });
});
$(document).on('click', '.scen-dl', function (e) {
  e.stopPropagation();
  Shiny.setInputValue('scenario_dl', Number($(this).closest('.scen-item').data('id')), {priority: 'event'});
});
$(document).on('click', '.scen-dlall', function () { Shiny.setInputValue('scenario_dl_all', Date.now(), {priority: 'event'}); });


// "Fix before running": the app lists what would stop a run (an assumption with bad values, an invalid number of trials) under the Run
// button. The button is held off while the list is not empty, and each line jumps to the thing to fix.
(function () {
  var blocked = false, REASON = 'Fix the items listed below first';
  function esc(t) { return $('<div>').text(t).html(); }
  function applyBlock() {
    var b = $('#run'); if (!b.length) return;
    b.toggleClass('blocked', blocked);
    if (blocked) b.prop('disabled', true).attr({'aria-disabled': 'true', title: REASON});
    else if (b.attr('title') === REASON) { b.prop('disabled', false).removeAttr('aria-disabled').attr('title', 'Shortcut: Cmd or Ctrl + Enter'); }
  }
  Shiny.addCustomMessageHandler('runIssues', function (m) {
    var items = m.items || [], box = $('#run_issues');
    blocked = items.length > 0;
    if (!blocked) box.empty();
    else {
      var shown = items.slice(0, 4), html = '<div class="ri-head">' + (items.length === 1 ? 'One thing to fix before running' : items.length + ' things to fix before running') + '</div><ul>';
      shown.forEach(function (it) { html += '<li><a href="#" class="ri-link" data-kind="' + esc(it.kind) + '" data-id="' + esc(it.id) + '">' + esc(it.label) + '</a>: ' + esc(it.msg) + '</li>'; });
      if (items.length > shown.length) html += '<li class="ri-more">and ' + (items.length - shown.length) + ' more</li>';
      box.html(html + '</ul>');
    }
    applyBlock();
  });
  // Something else (the run finishing) can re-enable the button; keep it held off while there is still something to fix
  $(document).on('shiny:idle shiny:value', function () { if (blocked) setTimeout(applyBlock, 0); });
  $(document).on('click', '.ri-link', function (e) {
    e.preventDefault();
    var kind = $(this).data('kind'), id = String($(this).data('id'));
    if (kind === 'field') { var f = document.getElementById(id); if (f) { f.scrollIntoView({block: 'center', behavior: 'smooth'}); f.focus(); } return; }
    if ($('#tabs li.active a').data('value') !== 'Define assumptions') $('#tabs a[data-value="Define assumptions"]').tab('show');
    setTimeout(function () {
      var card = $('#card_' + id); if (!card.length) return;
      if (!card.hasClass('open')) card.find('.assump-toggle').first().trigger('click');
      card[0].scrollIntoView({block: 'center', behavior: 'smooth'});
      card.addClass('focus-flash'); setTimeout(function () { card.removeClass('focus-flash'); }, 1600);
    }, 200);
  });
})();

// One-time tips on the charts that can be dragged. They go away with the x, or as soon as the chart is used, and stay away.
(function () {
  var KEY = 'vc_hints', seen = {};
  try { seen = JSON.parse(localStorage.getItem(KEY) || '{}') || {}; } catch (e) {}
  function dismiss(key) {
    var el = $('.chart-hint[data-hint="' + key + '"]'); if (!el.length || el.hasClass('gone')) return;
    el.addClass('gone'); setTimeout(function () { el.hide(); }, 400);
    seen[key] = 1; try { localStorage.setItem(KEY, JSON.stringify(seen)); } catch (e) {}
  }
  $(function () { $('.chart-hint').each(function () { if (seen[$(this).data('hint')]) $(this).hide(); }); });
  $(document).on('click', '.chart-hint-x', function () { dismiss($(this).closest('.chart-hint').data('hint')); });
  $(document).on('pointerup', '.tip-cell, .surv-wrap', function () {
    var card = $(this).closest('.plot-card'); var h = card.find('.chart-hint').first();
    if (h.length && ($(this).is('.surv-wrap') || $(this).find('#forecast_plot').length)) dismiss(h.data('hint'));
  });
})();


// Shadow above the Run / Past runs panel only while the settings are actually sliding underneath it: the panel is "stuck" when it sits
// higher than the place it would have in the normal flow, which a small marker placed just before it tells us.
(function () {
  $(function () {
    var dock = $('.sidebar-dock')[0]; if (!dock) return;
    var mark = document.createElement('div'); mark.className = 'dock-mark'; mark.style.cssText = 'height:0;margin:0;padding:0;border:0;';
    dock.parentNode.insertBefore(mark, dock);
    var scroller = dock.parentNode, queued = false;
    function check() {
      queued = false;
      // the marker sits where the panel would start, less the panel's top margin
      var natural = mark.getBoundingClientRect().bottom + (parseFloat(getComputedStyle(dock).marginTop) || 0);
      dock.classList.toggle('stuck', dock.getBoundingClientRect().top < natural - 1);
    }
    function queue() { if (!queued) { queued = true; requestAnimationFrame(check); setTimeout(check, 60); } }
    scroller.addEventListener('scroll', queue, {passive: true});
    window.addEventListener('resize', queue);
    if (window.ResizeObserver) { var ro = new ResizeObserver(queue); ro.observe(dock); ro.observe(scroller); }
    new MutationObserver(queue).observe(scroller, {childList: true, subtree: true, attributes: true});
    queue();
  });
})();


// Jump chips: the blue "active" fill is goo behind the translucent chips. When the active chip changes, a drop pulls out of the old chip,
// thins into a strand along the way, and swells into the new chip. Three shapes do it (the old chip's blue shrinking away, a thin strand,
// and the new chip's blue growing), and an SVG "goo" filter (blur, then sharpen the edge) fuses them into one stretchy shape.
(function () {
  var reduce = window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  var USE_BLOB = false;   // set to true to bring the oozing blob back; while false each chip just fades its blue on and off
  var D = 640, EASE = 'cubic-bezier(.45,0,.25,1)';
  function px(r) { return {left: r.left + 'px', top: r.top + 'px', width: r.w + 'px', height: r.h + 'px'}; }
  function dot(r, k) {                       // a small round drop at the centre of r, k times the chip height
    var d = r.h * k; return px({left: r.left + r.w / 2 - d / 2, top: r.top + r.h / 2 - d / 2, w: d, h: d});
  }
  function setup(bar) {
    if (!USE_BLOB) return;
    var goo = document.createElement('div'); goo.className = 'jump-goo'; goo.setAttribute('aria-hidden', 'true');
    var head = document.createElement('span'), tail = document.createElement('span'), strand = document.createElement('span');
    head.className = 'jb-head';
    goo.appendChild(tail); goo.appendChild(strand); goo.appendChild(head);
    bar.insertBefore(goo, bar.firstChild); bar.classList.add('has-blob');
    var last = null, anims = [];
    function rectOf(chip) { return {left: chip.offsetLeft, top: chip.offsetTop, w: chip.offsetWidth, h: chip.offsetHeight}; }
    function setStatic(r) { var s = px(r); for (var k in s) head.style[k] = s[k]; }
    function ooze(A, B) {
      anims.forEach(function (x) { x.cancel(); }); anims = [];
      var ax = A.left + A.w / 2, bx = B.left + B.w / 2, ay = A.top + A.h / 2, by = B.top + B.h / 2;
      var sh = Math.min(A.h, B.h) * .42, o = {duration: D, easing: EASE};
      var lo = Math.min(ax, bx), span = Math.abs(bx - ax);
      // the old chip's blue: full chip, then a drop, then gone
      anims.push(tail.animate([Object.assign(px(A)), Object.assign(dot(A, .55), {offset: .4}), Object.assign(dot(A, 0))], o));
      // the strand: grows out of the old chip to the new one, then is drawn into the new chip
      anims.push(strand.animate([
        {left: ax + 'px', top: ay - sh / 2 + 'px', width: '0px', height: sh + 'px'},
        {left: lo + 'px', top: (ay + by) / 2 - sh / 2 + 'px', width: span + 'px', height: sh + 'px', offset: .45},
        {left: bx + 'px', top: by - sh / 2 + 'px', width: '0px', height: sh + 'px'}], o));
      // the new chip's blue: a drop that travels over, then swells to fill the chip
      anims.push(head.animate([dot(A, .5), Object.assign(dot(B, .5), {offset: .45}), px(B)], o));
    }
    function place() {
      var chip = bar.querySelector('.jump-chip.active');
      if (!chip || !bar.offsetParent || !chip.offsetWidth) { head.classList.remove('on'); last = null; return; }
      var r = rectOf(chip);
      if (last && !reduce && head.animate && (Math.abs(last.left - r.left) > 1 || Math.abs(last.top - r.top) > 1)) ooze(last, r);
      else if (last && !reduce) { /* same chip, only its size changed */ }
      setStatic(r); head.classList.add('on'); last = r;
    }
    var queued = false;
    function queue() { if (!queued) { queued = true; requestAnimationFrame(function () { queued = false; place(); }); } }
    new MutationObserver(queue).observe(bar, {attributes: true, attributeFilter: ['class'], subtree: true});
    function snap() { anims.forEach(function (x) { x.cancel(); }); anims = []; last = null; queue(); }
    if (window.ResizeObserver) {
      var ro = new ResizeObserver(snap);     // a resize or a chip's count text changing moves the chips: snap to the new spot, no ooze
      ro.observe(bar); bar.querySelectorAll('.jump-chip').forEach(function (c) { ro.observe(c); });
    }
    window.addEventListener('resize', snap);
    queue();
  }
  $(function () {
    if (!USE_BLOB) return;
    // One shared SVG filter: blur the shapes together, then raise the contrast of the edge so the blur becomes a smooth, joined outline
    $('body').append('<svg width="0" height="0" style="position:absolute" aria-hidden="true" focusable="false"><defs>' +
      '<filter id="jump-goo" x="-10%" y="-60%" width="120%" height="220%" color-interpolation-filters="sRGB">' +
      '<feGaussianBlur in="SourceGraphic" stdDeviation="3.2" result="b"/>' +
      '<feColorMatrix in="b" mode="matrix" values="1 0 0 0 0  0 1 0 0 0  0 0 1 0 0  0 0 0 22 -9"/></filter></defs></svg>');
    $('.jump-bar').each(function () { setup(this); });
  });
})();


// ---- Polish: sliding tab marker, count-up numbers, plot wipe, card preview fade, card effect bars ----
(function () {
  var reduce = window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  var loadedAt = Date.now();

  // A marker on the top edge of the active main tab
  $(function () {
    var nav = document.getElementById('tabs'); if (!nav) return;
    var bar = document.createElement('div'); bar.className = 'tab-underline'; bar.setAttribute('aria-hidden', 'true'); nav.appendChild(bar);
    function place(animate, li) {
      li = li || nav.querySelector('li.active'); if (!li) return;
      if (!animate) bar.style.transition = 'none';
      // sized to the tab's own face (not the slot around it) and pulled in from the rounded corners
      var a = li.querySelector('a'), face = a ? a.offsetWidth : li.offsetWidth, inset = Math.min(10, face * 0.12);
      bar.style.left = (li.offsetLeft + inset) + 'px'; bar.style.width = Math.max(8, face - 2 * inset) + 'px'; bar.style.top = li.offsetTop + 'px';
      bar.classList.add('on');
      if (!animate) { void bar.offsetWidth; bar.style.transition = ''; }
    }
    // The marker sits on the active tab (no sliding). On a tab change it is put in place hidden and fades in with the tab's own face,
    // so it does not pop in ahead of the tab.
    $(document).on('show.bs.tab', '#tabs a[data-toggle="tab"]', function (e) {
      bar.style.transition = 'none'; bar.style.opacity = '0';
      place(false, e.target.parentNode);
      void bar.offsetWidth; bar.style.transition = ''; bar.style.opacity = '';
    });
    window.addEventListener('resize', function () { place(false); });
    if (window.ResizeObserver) new ResizeObserver(function () { place(false); }).observe(nav);
    if (document.fonts && document.fonts.ready) document.fonts.ready.then(function () { place(false); });
    setTimeout(function () { place(false); }, 300);
    place(false);
  });

  // Tile numbers tick from the previous value to the new one when a tile is redrawn with a different number
  var seen = {}, tok = /[-+]?\d[\d,]*(?:\.\d+)?/g;
  function tileKey(el) {
    var out = el.closest('.shiny-html-output'), tile = el.closest('.stat-tile');
    var lab = tile && tile.querySelector('.tile-label'), nm = tile && tile.querySelector('.tile-name');
    return (out ? out.id : '') + '|' + (lab ? lab.textContent : '') + '|' + (nm ? nm.textContent : '');
  }
  function fmtNum(v, like) {
    var m = like.match(/\.(\d+)/), d = m ? m[1].length : 0, body = Math.abs(v).toFixed(d);
    if (like.indexOf(',') >= 0) body = Math.abs(v).toLocaleString('en-US', {minimumFractionDigits: d, maximumFractionDigits: d});
    var sign = like.charAt(0) === '+' ? (v < 0 ? '-' : '+') : (v < 0 ? '-' : '');
    return sign + body;
  }
  function countUp(el, oldText, newText) {
    var a = oldText.match(tok), b = newText.match(tok);
    if (!a || !b || a.length !== b.length) return;
    var from = a.map(function (t) { return parseFloat(t.replace(/,/g, '')); }), to = b.map(function (t) { return parseFloat(t.replace(/,/g, '')); });
    var t0 = null, dur = 700;
    function frame(ts) {
      if (!el.isConnected) return;
      if (t0 === null) t0 = ts;
      var p = Math.min(1, (ts - t0) / dur), e = 1 - Math.pow(1 - p, 3), i = 0;
      el.textContent = p >= 1 ? newText : newText.replace(tok, function (t) { var k = i++; return fmtNum(from[k] + (to[k] - from[k]) * e, t); });
      if (p < 1) requestAnimationFrame(frame);
    }
    requestAnimationFrame(frame);
  }
  function handleTile(el) {
    if (el.__cu || el.children.length) return;
    el.__cu = true;
    var k = tileKey(el), txt = el.textContent, old = seen[k];
    seen[k] = txt;
    if (!reduce && old != null && old !== txt) PV_GATE.after(function () { countUp(el, old, txt); });
  }
  $(function () {
    new MutationObserver(function (muts) {
      muts.forEach(function (m) {
        Array.prototype.forEach.call(m.addedNodes, function (n) {
          if (n.nodeType !== 1) return;
          if (n.classList.contains('tile-value')) handleTile(n);
          else Array.prototype.forEach.call(n.querySelectorAll('.tile-value'), handleTile);
        });
      });
    }).observe(document.body, {childList: true, subtree: true});
    $('.tile-value').each(function () { handleTile(this); });
  });

  // After Run, the result charts are revealed left to right
  var WIPE = {}, wipeUntil = 0, wiped = {};   // (only the data of the forecast, sensitivity and survival charts animate; see the bar-chart block below)
  $(document).on('shiny:inputchanged', function (e) { if (e.name === 'run') { wipeUntil = Date.now() + 20000; wiped = {}; } });
  $(document).on('shiny:value', function (e) {
    var name = e.name;
    if (reduce) return;
    if (WIPE[name] && Date.now() < wipeUntil && !wiped[name]) {
      wiped[name] = true;
      setTimeout(function () {
        var el = document.getElementById(name); if (!el || !el.animate) return;
        el.animate([{clipPath: 'inset(0 100% 0 0)'}, {clipPath: 'inset(0 0 0 0)'}], {duration: 900, easing: 'cubic-bezier(.3,0,.2,1)'});
      }, 120);
    }
    // A card's distribution preview crossfades when you change its settings (not on the first load): a copy of the old picture is left on
    // top and fades away while the new one is already underneath, so it never dips. The copy lives in the card, not in the plot output,
    // because Shiny writes new pictures into every <img> it finds inside the output.
    if (/_prev$/.test(name) && Date.now() - loadedAt > 2500) {
      var el = document.getElementById(name), old = el && el.querySelector('img'), card = el && el.closest('.assump-card');
      if (!old || !card || !old.complete || !old.naturalWidth) return;
      var cr = card.getBoundingClientRect(), ir = old.getBoundingClientRect();
      if (el.__ghost) el.__ghost.remove();
      var ghost = document.createElement('img'); ghost.src = old.src; ghost.setAttribute('aria-hidden', 'true'); ghost.className = 'prev-ghost';
      ghost.style.cssText = 'position:absolute;pointer-events:none;opacity:1;transition:opacity .28s ease-out;z-index:2;left:' + (ir.left - cr.left - card.clientLeft) + 'px;top:' +
                            (ir.top - cr.top - card.clientTop) + 'px;width:' + ir.width + 'px;height:' + ir.height + 'px';
      card.appendChild(ghost); el.__ghost = ghost;
      var before = old.src, tries = 0;
      function done() { ghost.remove(); if (el.__ghost === ghost) el.__ghost = null; }
      (function wait() {
        var img = el.querySelector('img');
        if (img && img.complete && (img.src !== before || tries >= 4)) { requestAnimationFrame(function () { ghost.style.opacity = '0'; }); setTimeout(done, 400); }
        else if (++tries < 30) setTimeout(wait, 30); else done();
      })();
    }
  });

  // The effect-bar label names the newest run ("Run 3 effect on Ct", or whatever you renamed it to). When the name changes, the old text
  // fades out, the label eases to the new text's width, and the new text fades in, so the bar beside it slides smoothly instead of jumping.
  var runName = 'Last run';
  function effectText() { return runName + ' effect on Ct'; }
  function setEffectLabels(animate) {
    var text = effectText();
    document.querySelectorAll('.ce-label').forEach(function (l) {
      l.closest('.card-effect').setAttribute('data-run', runName);
      if (l.textContent === text) return;
      if (!animate || reduce || !l.animate || !l.textContent || !l.offsetWidth) { l.textContent = text; return; }
      var w0 = l.getBoundingClientRect().width;
      l.getAnimations().forEach(function (a) { a.cancel(); });
      var out = l.animate([{opacity: 1}, {opacity: 0}], {duration: 140, easing: 'ease-in', fill: 'forwards'});
      out.onfinish = function () {
        l.textContent = text; l.style.width = ''; var w1 = l.getBoundingClientRect().width;
        out.cancel();
        l.animate([{opacity: 0, width: w0 + 'px'}, {opacity: 1, width: w1 + 'px'}], {duration: 240, easing: 'ease-out'});
      };
    });
  }
  $(function () {
    if (!window.Shiny) return;
    Shiny.addCustomMessageHandler('runName', function (m) {
      var name = (m && m.name) || 'Last run', first = runName === 'Last run';
      if (name === runName) return;
      runName = name;
      // a new run's name arrives with its results: wait for the reveal. A rename is immediate.
      PV_GATE.after(function () { setEffectLabels(!first); });
    });
  });

  // A thin "effect on Ct" bar in each assumption card (the size of its partial rank correlation in the last run)
  $(function () {
    if (!window.Shiny) return;
    Shiny.addCustomMessageHandler('cardEffects', function (eff) {
      PV_GATE.after(function () { Object.keys(eff || {}).forEach(function (id) {
        var card = document.getElementById('card_' + id); if (!card) return;
        var row = card.querySelector('.card-effect');
        if (!row) {
          row = document.createElement('div'); row.className = 'card-effect';
          row.innerHTML = '<span class="ce-label"></span><span class="ce-track"><i></i></span>';
          var anchor = document.getElementById(id + '_prev') || card.querySelector('.assump-summary');
          if (anchor && anchor.parentNode) anchor.parentNode.insertBefore(row, anchor.nextSibling); else return;
        }
        var v = Number(eff[id]) || 0;
        row.querySelector('.ce-label').textContent = effectText(); row.setAttribute('data-run', runName);
        row.title = (v > 0 ? 'How strongly this assumption moved Ct in ' + runName + ', the most recent run (partial rank correlation ' + v.toFixed(2) + '). ' : 'This assumption was fixed in ' + runName + ', the most recent run, so it did not move Ct. ') +
                    'It is not recalculated as you edit: if you change this assumption the bar fades until you run again.';
        row.classList.toggle('none', v <= 0);
        var bar = row.querySelector('i'), t = Math.max(0, Math.min(1, v)), dark = document.documentElement.getAttribute('data-theme') === 'dark';
        bar.style.width = (t * 100) + '%';
        // the fuller the bar, the deeper (light theme) or brighter (dark theme) and more saturated its blue, so strong effects stand out
        bar.style.background = 'hsl(212,' + Math.round(42 + 48 * t) + '%,' + Math.round(dark ? 38 + 34 * t : 80 - 54 * t) + '%)';
      }); });
    });
  });
})();


// Only one of the About animations plays at a time: starting one pauses the others (they stay where they are and can be resumed)
window.pvDemos = [];
window.pvPauseOthers = function (root) { window.pvDemos.forEach(function (d) { if (d.root !== root) d.pause(); }); };

// Run a callback once an element fills the majority of the scrolling view (like the jump chips, which switch when a section does), so
// animations on a long page start when you have actually scrolled to them, not when their edge first peeks in.
window.pvWhenInView = function (el, cb) {
  var done = false, queued = false;
  function scroller() { return window.innerWidth >= 768 ? document.querySelector('.tab-content') : null; }
  function check() {
    queued = false;
    if (done || !el.offsetParent) return;
    var sc = scroller(), base = sc ? sc.getBoundingClientRect().top : 0, top = base + 50, bottom = base + (sc ? sc.clientHeight : window.innerHeight);
    var r = el.getBoundingClientRect(), vis = Math.min(r.bottom, bottom) - Math.max(r.top, top);
    if (vis >= 0.5 * (bottom - top) || vis >= 0.9 * r.height) { done = true; document.removeEventListener('scroll', q, true); cb(); }
  }
  function q() { if (!queued && !done) { queued = true; requestAnimationFrame(check); } }
  document.addEventListener('scroll', q, true); window.addEventListener('resize', q); $(document).on('shown.bs.tab', q); setTimeout(check, 500);
};


// ---- Cohort animation (About tab): 240 dots die off under the chosen mortality model, with the survivorship curve beside them ----
(function () {
  $(function () {
    var root = document.getElementById('cohort'); if (!root) return;
    var lx = {}; try { lx = JSON.parse(root.getAttribute('data-lx')); } catch (e) { return; }
    var cv = root.querySelector('canvas'), ctx = cv.getContext('2d'), playBtn = root.querySelector('.cohort-play'),
        scrub = root.querySelector('.cohort-scrub'), read = root.querySelector('.cohort-read');
    var reduce = window.matchMedia('(prefers-reduced-motion: reduce)').matches;
    var COLS = 24, ROWS = 10, N = COLS * ROWS, SECS = 11;
    var model = 'logistic', curve, maxDay, death, t = 0, playing = false, last = 0, started = false, raf = 0, W = 0, H = 0, dpr = 1;

    function rng(seed) { return function () { seed |= 0; seed = seed + 0x6D2B79F5 | 0; var x = Math.imul(seed ^ seed >>> 15, 1 | seed); x = x + Math.imul(x ^ x >>> 7, 61 | x) ^ x; return ((x ^ x >>> 14) >>> 0) / 4294967296; }; }
    function surv(tt) { var i = Math.min(curve.length - 1, Math.floor(tt)), j = Math.min(curve.length - 1, i + 1), f = tt - i; return curve[i] + (curve[j] - curve[i]) * f; }
    function timeFor(v) {                         // the time at which the share alive falls to v
      for (var i = 0; i < curve.length - 1; i++) if (curve[i] >= v && curve[i + 1] < v) return i + (curve[i] - v) / Math.max(1e-9, curve[i] - curve[i + 1]);
      return Infinity;
    }
    function build(m) {
      model = m; curve = lx[m]; maxDay = curve.length - 1;
      for (var i = curve.length - 1; i > 0; i--) if (curve[i] >= 0.01) { maxDay = Math.min(curve.length - 1, i + 2); break; }
      var order = []; for (var k = 0; k < N; k++) order.push(k);
      var r = rng(12345); for (k = N - 1; k > 0; k--) { var j = Math.floor(r() * (k + 1)), tmp = order[k]; order[k] = order[j]; order[j] = tmp; }
      death = new Array(N);
      for (k = 0; k < N; k++) death[order[k]] = timeFor((N - k - 0.5) / N);   // the k-th mosquito to die takes a random place in the grid
      t = 0; scrub.value = 0;
    }
    function median() { var d = timeFor(0.5); return isFinite(d) ? d : null; }
    function color(name, fb) { var v = getComputedStyle(document.documentElement).getPropertyValue(name).trim(); return v || fb; }

    function size() {
      var pcs = getComputedStyle(cv.parentNode), w = (cv.parentNode.clientWidth || root.clientWidth) - (parseFloat(pcs.paddingLeft) || 0) - (parseFloat(pcs.paddingRight) || 0); if (!(w > 0)) return false;   // the box's inner width, not including its padding
      dpr = window.devicePixelRatio || 1; W = w; H = w < 560 ? 300 : 200;
      cv.style.width = W + 'px'; cv.style.height = H + 'px'; cv.width = Math.round(W * dpr); cv.height = Math.round(H * dpr);
      return true;
    }
    function draw() {
      if (!W && !size()) return;
      ctx.setTransform(dpr, 0, 0, dpr, 0, 0); ctx.clearRect(0, 0, W, H);
      var alive = color('--btn', '#2b6cb0'), gone = color('--muted', '#8a94a0'), line = color('--line-strong', '#c8d0d9'), accent = color('--accent', '#2b6cb0'), txt = color('--muted', '#6b7785');
      var stack = W < 560, dotsW = stack ? W : Math.round(W * 0.56), dotsH = stack ? 150 : H;
      var sx = dotsW / COLS, sy = dotsH / ROWS, rad = Math.min(sx, sy) * 0.34, k, x, y;
      for (k = 0; k < N; k++) {
        x = (k % COLS + 0.5) * sx; y = (Math.floor(k / COLS) + 0.5) * sy;
        var d = death[k], since = t - d;
        if (t < d) { ctx.globalAlpha = 1; ctx.fillStyle = alive; ctx.beginPath(); ctx.arc(x, y, rad, 0, 6.2832); ctx.fill(); }
        else if (since < 0.9 && !reduce) { var q = since / 0.9; ctx.globalAlpha = 1 - q; ctx.fillStyle = gone; ctx.beginPath(); ctx.arc(x, y + q * rad * 1.2, rad * (1 - 0.5 * q), 0, 6.2832); ctx.fill(); }
      }
      ctx.globalAlpha = 1;
      // the survivorship curve
      var gx = stack ? 36 : dotsW + 40, gy = stack ? dotsH + 14 : 12, gw = W - gx - 12, gh = (stack ? H : H) - gy - 26;
      ctx.font = '11px system-ui, sans-serif'; ctx.fillStyle = txt; ctx.strokeStyle = line; ctx.lineWidth = 1;
      ctx.beginPath(); ctx.moveTo(gx, gy); ctx.lineTo(gx, gy + gh); ctx.lineTo(gx + gw, gy + gh); ctx.stroke();
      ctx.textAlign = 'right'; ctx.textBaseline = 'middle'; ctx.fillText('100%', gx - 5, gy); ctx.fillText('0', gx - 5, gy + gh);
      ctx.textAlign = 'center'; ctx.textBaseline = 'top';
      var step = maxDay > 60 ? 20 : 10; for (var dd = 0; dd <= maxDay; dd += step) ctx.fillText(dd, gx + gw * dd / maxDay, gy + gh + 5);
      ctx.fillText('Day', gx + gw / 2, gy + gh + 15);
      function px(dayv) { return gx + gw * dayv / maxDay; } function py(v) { return gy + gh * (1 - v); }
      ctx.strokeStyle = line; ctx.beginPath(); for (k = 0; k <= maxDay; k++) { if (k === 0) ctx.moveTo(px(k), py(curve[k])); else ctx.lineTo(px(k), py(curve[k])); } ctx.stroke();
      ctx.strokeStyle = accent; ctx.lineWidth = 2.2; ctx.beginPath(); ctx.moveTo(px(0), py(curve[0]));
      for (k = 1; k <= Math.floor(t); k++) ctx.lineTo(px(k), py(curve[k])); ctx.lineTo(px(t), py(surv(t))); ctx.stroke();
      // the moving point, with a soft glow
      var mx = px(t), my = py(surv(t)), halo = ctx.createRadialGradient(mx, my, 0, mx, my, 13);
      halo.addColorStop(0, accent); halo.addColorStop(1, 'rgba(0,0,0,0)');
      ctx.globalAlpha = 0.28; ctx.fillStyle = halo; ctx.beginPath(); ctx.arc(mx, my, 13, 0, 6.2832); ctx.fill(); ctx.globalAlpha = 1;
      ctx.shadowColor = accent; ctx.shadowBlur = 10; ctx.fillStyle = accent; ctx.beginPath(); ctx.arc(mx, my, 4, 0, 6.2832); ctx.fill();
      ctx.shadowBlur = 0; ctx.shadowColor = 'transparent';
      var f = surv(t), md = median();
      read.textContent = 'Day ' + Math.round(t) + ' · ' + Math.round(f * 100) + '% alive' + (md !== null ? ' · half have died by day ' + Math.round(md) : '');
      cv.setAttribute('aria-label', 'Dots for ' + N + ' mosquitoes dying off under the ' + model + ' model. Day ' + Math.round(t) + ': ' + Math.round(f * 100) + '% alive.');
      scrub.value = Math.round(t / maxDay * 1000);
    }
    function setPlaying(on) {
      playing = on; playBtn.innerHTML = '<i class="fa fa-' + (on ? 'pause' : (t >= maxDay ? 'rotate-left' : 'play')) + '" role="presentation"></i> ' + (on ? 'Pause' : (t >= maxDay ? 'Replay' : 'Play'));
      if (on) { window.pvPauseOthers(root); last = 0; cancelAnimationFrame(raf); raf = requestAnimationFrame(tick); }
    }
    window.pvDemos.push({root: root, pause: function () { if (playing) setPlaying(false); }});
    function tick(ts) {
      if (!playing) return;
      if (!last) last = ts;
      t = Math.min(maxDay, t + (ts - last) / 1000 * (maxDay / SECS)); last = ts; draw();
      if (t >= maxDay) { setPlaying(false); draw(); return; }
      raf = requestAnimationFrame(tick);
    }
    playBtn.addEventListener('click', function () { if (!playing && t >= maxDay) t = 0; setPlaying(!playing); started = true; if (!playing) draw(); });
    scrub.addEventListener('input', function () { t = scrub.value / 1000 * maxDay; setPlaying(false); draw(); });
    root.querySelectorAll('.cohort-m').forEach(function (b) {
      b.addEventListener('click', function () {
        root.querySelectorAll('.cohort-m').forEach(function (x) { var on = x === b; x.classList.toggle('on', on); x.setAttribute('aria-pressed', on ? 'true' : 'false'); });
        build(b.getAttribute('data-model')); started = true; if (reduce) { setPlaying(false); draw(); } else { setPlaying(true); }
      });
    });
    if (window.ResizeObserver) new ResizeObserver(function () { W = 0; draw(); }).observe(cv.parentNode);
    new MutationObserver(function () { draw(); }).observe(document.documentElement, {attributes: true, attributeFilter: ['data-theme']});
    build('logistic'); setPlaying(false); draw();
    // Start by itself once it fills most of the screen
    if (!reduce) window.pvWhenInView(root, function () { if (!started) { started = true; setPlaying(true); } });
  });
})();


// Cards in the same row: even out the title and summary heights so every preview plot and effect bar lines up across the row
(function () {
  function align() {
    document.querySelectorAll('.cards-grid').forEach(function (grid) {
      var cards = Array.prototype.filter.call(grid.querySelectorAll('.assump-card'), function (c) { return c.offsetParent !== null; });
      if (!cards.length) return;
      var parts = cards.map(function (c) { return [c.querySelector('.assump-head'), c.querySelector('.assump-summary')]; });
      parts.forEach(function (p) { p.forEach(function (el) { if (el) el.style.minHeight = ''; }); });    // measure at natural height first
      var rows = {};
      cards.forEach(function (c, i) {
        var key = Math.round(c.offsetTop), h = parts[i][0] ? parts[i][0].offsetHeight : 0, sm = parts[i][1] ? parts[i][1].offsetHeight : 0;
        var r = rows[key] || (rows[key] = {h: 0, s: 0, idx: []}); r.h = Math.max(r.h, h); r.s = Math.max(r.s, sm); r.idx.push(i);
      });
      Object.keys(rows).forEach(function (k) {
        var r = rows[k]; if (r.idx.length < 2) return;
        r.idx.forEach(function (i) {
          if (parts[i][0]) parts[i][0].style.minHeight = r.h + 'px';
          if (parts[i][1]) parts[i][1].style.minHeight = r.s + 'px';
        });
      });
    });
  }
  var queued = false;
  function queue() { if (!queued) { queued = true; requestAnimationFrame(function () { queued = false; align(); }); } }
  $(function () {
    document.querySelectorAll('.cards-grid').forEach(function (g) {
      if (window.ResizeObserver) new ResizeObserver(queue).observe(g);
      // titles or summaries changing text (a new preset, mortality model, or distribution) can change their height
      new MutationObserver(function (muts) {
        if (muts.some(function (m) { return !(m.target.style && m.attributeName === 'style'); })) queue();
      }).observe(g, {childList: true, subtree: true, characterData: true, attributes: true, attributeFilter: ['class']});
    });
    window.addEventListener('resize', queue);
    $(document).on('shown.bs.tab', queue);
    if (document.fonts && document.fonts.ready) document.fonts.ready.then(queue);
    setTimeout(queue, 400); queue();
  });
})();


// ---- Bar charts draw in after a run: forecast bars rise one after another; sensitivity bars grow out of the centre line together ----
// The charts are pictures, so each bar is hidden by a hole cut in the picture's clip-path (the card shows through) and the holes shrink.
// The bar positions come from the server (hoverBars) and the picture's coordinate map from Shiny, the same data the hover tooltips use.
(function () {
  var reduce = window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  var CHARTS = {forecast_plot: 1, sens_plot: 1, surv_plot_s: 1, surv_plot_h: 1};     // the last two have no bars: their lines zip across
  var LINES = {surv_plot_s: 1, surv_plot_h: 1};
  var fresh = {};                                // charts that should draw in the next time they are drawn (set by Run, used up once)
  var barsSeq = 0, runSeq = {}, bars = {}, barsAt = {}, maps = {};
  // The run the app does by itself when the page opens counts as a run too (it sends no 'run' event), so every chart's first drawing animates
  Object.keys(CHARTS).forEach(function (n) { fresh[n] = true; runSeq[n] = 0; });
  function ease(p) { p = Math.max(0, Math.min(1, p)); return 1 - Math.pow(1 - p, 3); }

  $(document).on('shiny:inputchanged', function (e) { if (e.name === 'run') { Object.keys(CHARTS).forEach(function (n) { fresh[n] = true; runSeq[n] = barsSeq; }); } });
  // (the hover tooltips own the 'hoverBars' message; they announce each one here)
  $(document).on('pv:bars', function (e, m) { barsSeq++; bars[m.chart] = m.bars || []; barsAt[m.chart] = barsSeq; });
  $(document).on('shiny:value', function (e) {
    var name = e.name; if (!CHARTS[name]) return;
    maps[name] = e.value && e.value.coordmap;
    if (reduce || !fresh[name]) return;
    var el = document.getElementById(name), im = el && el.querySelector('img'); if (!el) return;
    el.style.visibility = 'hidden';              // no flash of the finished chart while the bar positions arrive
    PV_GATE.after(function () {
    var tries = 0, seenAt = Date.now();
    (function wait() {
      // (an identical re-run gives an identical picture, so "has the picture changed" cannot be used; a short wait lets the new one land)
      var img = el.querySelector('img'), ready = Date.now() - seenAt > 150 && img && img.complete && img.naturalWidth && (LINES[name] || barsAt[name] > (runSeq[name] || 0)) && maps[name];
      if (ready) { fresh[name] = false; start(name, el, img); return; }
      if (++tries > 40) { fresh[name] = false; el.style.visibility = ''; return; }     // give up after ~2 s: just show the chart
      setTimeout(wait, 50);
    })();
    });
  });

  // Every time a tab is opened its charts draw in again. The data is hidden the moment the tab is clicked (so the finished chart does not
  // flash while the tab fades in), then drawn in once the tab is showing. The first view after a run is handled by the code above instead.
  var replay = {};
  $(document).on('show.bs.tab', '#tabs a[data-toggle="tab"]', function (e) {
    if (reduce || PV_GATE.on) return;
    var href = e.target.getAttribute('href'), pane = href && href.charAt(0) === '#' ? document.querySelector(href) : null; replay = {};
    if (!pane) return;
    Object.keys(CHARTS).forEach(function (name) {
      var el = pane.querySelector('#' + name), img = el && el.querySelector('img');
      if (!el || !img || fresh[name] || !maps[name]) return;
      replay[name] = true; el.style.visibility = 'hidden';
      setTimeout(function () { if (replay[name]) { replay[name] = false; el.style.visibility = ''; } }, 2500);    // safety: never leave a chart hidden
    });
  });
  $(document).on('shown.bs.tab', '#tabs a[data-toggle="tab"]', function () {
    Object.keys(replay).forEach(function (name) {
      if (!replay[name]) return; replay[name] = false;
      var el = document.getElementById(name), img = el && el.querySelector('img');
      if (img && img.complete && img.naturalWidth) start(name, el, img); else if (el) el.style.visibility = '';
    });
  });

  function start(name, el, img) {
    var map = maps[name], panel = map && map.panels && map.panels[0], list = bars[name];
    if (!panel || (!LINES[name] && (!list || !list.length)) || !img.getBoundingClientRect().width) { el.style.visibility = ''; return; }
    var ir = img.getBoundingClientRect(), W = ir.width, H = ir.height, sx = map.dims.width / W, sy = map.dims.height / H;
    var d = panel.domain, g = panel.range;
    function cx(v) { return (g.left + (v - d.left) / (d.right - d.left) * (g.right - g.left)) / sx; }
    function cy(v) { return (g.bottom - (v - d.bottom) / (d.top - d.bottom) * (g.bottom - g.top)) / sy; }
    var isSens = name === 'sens_plot', isLines = !!LINES[name], L = g.left / sx, R = g.right / sx, DUR = isLines ? 850 : isSens ? 1150 : 1500, t0 = null;
    var n = list ? list.length : 0;
    function rect(x, y, w, h) { return w > 0.2 && h > 0.2 ? 'M' + x.toFixed(1) + ' ' + y.toFixed(1) + 'h' + w.toFixed(1) + 'v' + h.toFixed(1) + 'h' + (-w).toFixed(1) + 'z' : ''; }
    function render(t) {
      var holes = '';
      if (isLines) {
        // Only the plot area is covered, so the axes and labels stay. The edge of the reveal is slanted, so the lines near the top
        // of the chart are drawn a moment before the ones lower down, as if each run's line were zipping across.
        var T = g.top / sy, B = g.bottom / sy, skew = Math.min(160, (R - L) * 0.22), e = ease(t);
        var front = L - skew + e * (R - L + skew);
        function cl(v) { return Math.max(L, Math.min(R + 2, v)); }
        holes = 'M' + cl(front).toFixed(1) + ' ' + (T - 2).toFixed(1) + 'L' + (R + 2).toFixed(1) + ' ' + (T - 2).toFixed(1) + 'L' + (R + 2).toFixed(1) + ' ' + (B + 2).toFixed(1) +
                'L' + cl(front + skew).toFixed(1) + ' ' + (B + 2).toFixed(1) + 'z';
        img.style.clipPath = t >= 1 ? '' : 'path(evenodd,"M0 0H' + W.toFixed(1) + 'V' + H.toFixed(1) + 'H0z' + holes + '")';
        return;
      }
      list.forEach(function (b, i) {
        var x0 = cx(b.x0), x1 = cx(b.x1), top = cy(b.y1), bot = cy(b.y0);
        if (!isSens) {                           // histogram: bar i starts a little after bar i-1 and rises from the baseline
          var e = ease((t - (i / n) * 0.6) / 0.4);
          holes += rect(x0, top - 1, x1 - x0, (bot - top) * (1 - e) + 1);
        } else {                                 // sensitivity: out from the zero line, then the interval and the value label
          var c = cx(0), pos = b.x1 > 0 && b.x0 >= 0, end = pos ? x1 : x0, e1 = ease(t / 0.62), p = ease((t - 0.62) / 0.38), pad = 3;
          var y = top - pad, h = bot - top + 2 * pad, sx0;
          if (pos) { sx0 = e1 < 1 ? c + 1 + e1 * (end - c - 1) : end + p * (R + 24 - end); holes += rect(sx0, y, R + 24 - sx0, h); }
          else { sx0 = e1 < 1 ? c - 1 + e1 * (end - c + 1) : end - p * (end - L); holes += rect(L, y, sx0 - L, h); }
        }
      });
      img.style.clipPath = t >= 1 ? '' : 'path(evenodd,"M0 0H' + W.toFixed(1) + 'V' + H.toFixed(1) + 'H0z' + holes + '")';
    }
    function frame(ts) {
      if (!img.isConnected) { img = el.querySelector('img'); if (!img) return; }      // Shiny swapped the picture: carry on with the new one
      if (t0 === null) t0 = ts;
      var t = Math.min(1, (ts - t0) / DUR); render(t);
      if (t < 1) requestAnimationFrame(frame);
    }
    render(0);                                   // every bar hidden before the chart is shown, so there is no flash of the finished chart
    el.style.visibility = '';
    requestAnimationFrame(frame);
  }
})();


// ---- Skeleton shown in place of the results while the Run bar fills (see PV_GATE at the top) ----
$(function () {
  $('.results-body').each(function () {
    if (this.querySelector(':scope > .pv-skel')) return;
    var sk = document.createElement('div'); sk.className = 'pv-skel'; sk.setAttribute('aria-hidden', 'true');
    sk.innerHTML = '<div class="sk-line sk-w40"></div><div class="sk-tiles"><i></i><i></i><i></i><i></i></div><div class="sk-line sk-w70"></div><div class="sk-chart"></div><div class="sk-line sk-w55"></div>';
    this.insertBefore(sk, this.firstChild);
  });
});


// Reset pop-up: "Start over" swaps to its confirmation inside the same dialog (a second dialog would flash the backdrop)
$(document).on('click', '#start_over', function () {
  var m = $(this).closest('.modal'); m.find('.reset-panes').addClass('confirming'); m.find('.modal-title').text('Start over?');
});
$(document).on('click', '#reset_back', function () {
  var m = $(this).closest('.modal'); m.find('.reset-panes').removeClass('confirming'); m.find('.modal-title').text('Reset');
});


// Quick summary: the run details share its bar, so a click anywhere on the bar (but not on the Re-run link) opens or closes it
$(document).on('click', '.insight-bar', function (e) {
  if ($(e.target).closest('a, button').length) return;
  $(this).find('.insight-head').trigger('click');
});


// ---- Past runs header: a small line of median Ct across your runs (oldest to newest), so you can see which way your changes are pushing it ----
$(function () {
  if (!window.Shiny) return;
  var reduce = window.matchMedia('(prefers-reduced-motion: reduce)').matches, W = 58, H = 20, PAD = 3;
  function fmt(v) { return String(parseFloat(v.toPrecision(3))); }
  function draw(runs) {
    var row = document.querySelector('.history-title-row'); if (!row) return;
    var el = row.querySelector('.run-spark');
    var pts = (runs || []).filter(function (r) { return isFinite(Number(r.med)); });
    if (pts.length < 2) { if (el) el.remove(); return; }              // one run has no trend to show
    if (!el) { el = document.createElement('span'); el.className = 'run-spark'; row.appendChild(el); }
    var v = pts.map(function (r) { return Number(r.med); }), lo = Math.min.apply(null, v), hi = Math.max.apply(null, v), span = hi - lo;
    var xs = function (i) { return PAD + (W - 2 * PAD) * i / (v.length - 1); };
    var ys = function (x) { return span > 0 ? H - PAD - (H - 2 * PAD) * (x - lo) / span : H / 2; };
    var d = v.map(function (x, i) { return (i ? 'L' : 'M') + xs(i).toFixed(1) + ' ' + ys(x).toFixed(1); }).join('');
    var lx = xs(v.length - 1), ly = ys(v[v.length - 1]), up = v[v.length - 1] > v[v.length - 2], flat = v[v.length - 1] === v[v.length - 2];
    el.className = 'run-spark ' + (flat ? 'flat' : up ? 'up' : 'down');
    el.innerHTML = '<svg width="' + W + '" height="' + H + '" viewBox="0 0 ' + W + ' ' + H + '" aria-hidden="true" focusable="false">' +
      '<path class="rs-line" d="' + d + '" pathLength="100"/><circle class="rs-dot" cx="' + lx.toFixed(1) + '" cy="' + ly.toFixed(1) + '" r="2.6"/></svg>';
    var label = 'Median Ct by run: ' + pts.map(function (r) { return r.name + ' ' + fmt(Number(r.med)); }).join(', ');
    el.setAttribute('title', label); el.setAttribute('role', 'img'); el.setAttribute('aria-label', label);
    if (!reduce) { var line = el.querySelector('.rs-line'); if (line.animate) line.animate([{strokeDashoffset: 100}, {strokeDashoffset: 0}], {duration: 600, easing: 'ease-out'}); }
  }
  Shiny.addCustomMessageHandler('runSeries', function (m) { PV_GATE.after(function () { draw(m && m.runs); }); });
});


// ---- About tab: two more animations (age structure, incubation race) ----
(function () {
  var reduce = window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  function cvar(name, fb) { var v = getComputedStyle(document.documentElement).getPropertyValue(name).trim(); return v || fb; }
  function rng(seed) { return function () { seed |= 0; seed = seed + 0x6D2B79F5 | 0; var x = Math.imul(seed ^ seed >>> 15, 1 | seed); x = x + Math.imul(x ^ x >>> 7, 61 | x) ^ x; return ((x ^ x >>> 14) >>> 0) / 4294967296; }; }
  function blend(ctx, c1, c2, f) {
    function rgb(c) { ctx.fillStyle = '#000'; ctx.fillStyle = c; var h = ctx.fillStyle; return [parseInt(h.substr(1, 2), 16), parseInt(h.substr(3, 2), 16), parseInt(h.substr(5, 2), 16)]; }
    var a = rgb(c1), b = rgb(c2); return 'rgb(' + a.map(function (v, i) { return Math.round(v + (b[i] - v) * f); }).join(',') + ')';
  }
  function interp(curve, tt) { var i = Math.max(0, Math.min(curve.length - 1, Math.floor(tt))), j = Math.min(curve.length - 1, i + 1); return curve[i] + (curve[j] - curve[i]) * (tt - i); }
  function timeFor(curve, v) { for (var i = 0; i < curve.length - 1; i++) if (curve[i] >= v && curve[i + 1] < v) return i + (curve[i] - v) / Math.max(1e-9, curve[i] - curve[i + 1]); return Infinity; }
  function sizeCanvas(cv, hWide, hNarrow) {
    var pcs = getComputedStyle(cv.parentNode), w = cv.parentNode.clientWidth - (parseFloat(pcs.paddingLeft) || 0) - (parseFloat(pcs.paddingRight) || 0); if (!(w > 0)) return null;   // the box's inner width, not including its padding
    var dpr = window.devicePixelRatio || 1, h = w < 560 ? hNarrow : hWide;
    cv.style.width = w + 'px'; cv.style.height = h + 'px'; cv.width = Math.round(w * dpr); cv.height = Math.round(h * dpr);
    return {w: w, h: h, dpr: dpr};
  }

  // Shared play / scrub / model-button plumbing for both widgets
  function wire(root, o) {
    var playBtn = root.querySelector('.cohort-play'), playing = false, last = 0, raf = 0, started = false;
    o.state.t = 0;
    function label() { if (!playBtn) return; playBtn.innerHTML = '<i class="fa fa-' + (playing ? 'pause' : (o.state.t >= o.state.max ? 'rotate-left' : 'play')) + '" role="presentation"></i> ' + (playing ? 'Pause' : (o.state.t >= o.state.max ? 'Replay' : 'Play')); }
    function set(on) { playing = on; label(); if (on) { window.pvPauseOthers(root); last = 0; cancelAnimationFrame(raf); raf = requestAnimationFrame(tick); } }
    window.pvDemos.push({root: root, pause: function () { if (playing) set(false); }});
    function tick(ts) {
      if (!playing) return; if (!last) last = ts;
      o.state.t = Math.min(o.state.max, o.state.t + (ts - last) / 1000 * (o.state.max / o.secs())); last = ts; o.draw();
      if (o.state.t >= o.state.max) { set(false); o.draw(); return; }
      raf = requestAnimationFrame(tick);
    }
    if (playBtn) playBtn.addEventListener('click', function () { if (!playing && o.state.t >= o.state.max) o.state.t = 0; started = true; set(!playing); if (!playing) o.draw(); });   // (the age-structure figure has no button: it just draws in)
    root.querySelectorAll('.cohort-m').forEach(function (b) {
      b.addEventListener('click', function () {
        root.querySelectorAll('.cohort-m').forEach(function (x) { var on = x === b; x.classList.toggle('on', on); x.setAttribute('aria-pressed', on ? 'true' : 'false'); });
        o.setModel(b.getAttribute('data-model')); o.state.t = 0; started = true; if (reduce) { o.state.t = o.state.max; set(false); o.draw(); } else set(true);
      });
    });
    if (window.ResizeObserver) new ResizeObserver(function () { o.resize(); o.draw(); }).observe(o.canvas.parentNode);
    new MutationObserver(function () { o.draw(); }).observe(document.documentElement, {attributes: true, attributeFilter: ['data-theme']});
    if (!reduce) window.pvWhenInView(root, function () { if (!started) { started = true; o.state.t = 0; set(true); } });   // starts once it fills most of the screen
    return {play: function () { o.state.t = 0; set(true); }, label: label};
  }

  // ---- Synchronous vs stable age structure, as in Styer et al. (2007): the share of the population at each age, and the resulting Ct ----
  // Synchronous emergence is a population entirely of young adults, spread equally over ages 3 to 6 days (Ct is averaged over those ages).
  // The stable age distribution has adults emerging continuously: the share at age x is l(x) e^(-r x), scaled to add up to 100%.
  $(function () {
    var root = document.getElementById('agedemo'); if (!root) return;
    var data; try { data = JSON.parse(root.getAttribute('data-age')); } catch (e) { return; }
    var cv = root.querySelector('canvas'), ctx = cv.getContext('2d'), read = root.querySelector('.cohort-read');
    var MAXD = 30, YMAX = 0.27, model = 'logistic', S = {t: 0, max: 1}, dim = null;
    function ease(x) { x = Math.max(0, Math.min(1, x)); return 1 - Math.pow(1 - x, 3); }
    function resize() { dim = sizeCanvas(cv, 208, 330); }
    function draw() {
      if (!dim) resize(); if (!dim) return;
      var W = dim.w, H = dim.h, p = S.t, w = data.w[model], side = W >= 560, a;
      ctx.setTransform(dim.dpr, 0, 0, dim.dpr, 0, 0); ctx.clearRect(0, 0, W, H);
      var btn = cvar('--btn', '#2b6cb0'), accent = cvar('--accent', '#2b6cb0'), line = cvar('--line-strong', '#c8d0d9'), grid = cvar('--line', '#e3e8ee'), muted = cvar('--muted', '#6b7785'), txt = cvar('--text', '#1f2933');
      // Side by side on a wide screen (same scale and age axis in both), stacked on a narrow one
      var hP = side ? 92 : 78, top0 = side ? 58 : 38, yLab = side ? 36 : 44, pad = side ? 14 : 14;
      var geo = side ? [{x0: yLab, x1: W / 2 - pad, top: top0}, {x0: W / 2 + yLab, x1: W - pad, top: top0}]
                     : [{x0: yLab, x1: W - pad, top: top0}, {x0: yLab, x1: W - pad, top: top0 + hP + 50}];
      var panels = [{title: 'Synchronous emergence', sub: 'entirely young: ages 3 to 6 days, equally', col: accent, ct: data.ct.sync[model]},
                    {title: 'Stable age distribution', sub: 'adults keep emerging, so every age is present', col: accent, ct: data.ct.stable[model]}];
      panels.forEach(function (pn, i) {
        pn.x0 = geo[i].x0; pn.x1 = geo[i].x1; pn.top = geo[i].top; pn.bot = pn.top + hP; pn.slot = (pn.x1 - pn.x0) / (MAXD + 1); pn.bw = Math.max(2, pn.slot * 0.8);
        pn.X = function (age) { return pn.x0 + pn.slot * (age + 0.5); };
        ctx.textAlign = 'left'; ctx.textBaseline = 'alphabetic'; ctx.font = '600 12.5px system-ui, sans-serif'; ctx.fillStyle = txt;
        var tw = ctx.measureText(pn.title).width;
        if (side) { ctx.fillText(pn.title, pn.x0 - yLab + 4, 20); ctx.font = '12px system-ui, sans-serif'; ctx.fillStyle = muted; ctx.fillText(pn.sub, pn.x0 - yLab + 4, 38); }
        else { ctx.fillText(pn.title, pn.x0, pn.top - 12); ctx.font = '12px system-ui, sans-serif'; ctx.fillStyle = muted; ctx.fillText(pn.sub, pn.x0 + tw + 10, pn.top - 12); }
        // gridlines and share labels
        ctx.lineWidth = 1; ctx.font = '10.5px system-ui, sans-serif'; ctx.textAlign = 'right'; ctx.textBaseline = 'middle';
        [0, 0.1, 0.2].forEach(function (v) { var y = pn.bot - hP * v / YMAX; ctx.strokeStyle = v === 0 ? line : grid; ctx.beginPath(); ctx.moveTo(pn.x0, y); ctx.lineTo(pn.x1, y); ctx.stroke(); ctx.fillStyle = muted; ctx.fillText(Math.round(v * 100) + '%', pn.x0 - 6, y); });
      });
      // age axis under each panel (side by side) or under the lower one (stacked)
      ctx.textAlign = 'center'; ctx.textBaseline = 'top'; ctx.font = '11px system-ui, sans-serif'; ctx.fillStyle = muted; ctx.strokeStyle = line;
      (side ? panels : [panels[1]]).forEach(function (pn) {
        for (a = 0; a <= MAXD; a += 10) { ctx.fillText(a, pn.X(a), pn.bot + 5); ctx.beginPath(); ctx.moveTo(pn.X(a), pn.bot); ctx.lineTo(pn.X(a), pn.bot + 3); ctx.stroke(); }
        ctx.fillText('Age (days)', (pn.x0 + pn.x1) / 2, pn.bot + 19);
      });
      // bars: synchronous first, then the stable mix filling in from the youngest age
      var ps = panels[0], pt = panels[1];
      ctx.fillStyle = accent; for (a = 3; a <= 6; a++) { var h1 = hP * 0.25 / YMAX * ease(p * 2.2); ctx.fillRect(ps.X(a) - ps.bw / 2, ps.bot - h1, ps.bw, h1); }
      ctx.fillStyle = accent; for (a = 0; a <= MAXD; a++) { var h2 = hP * w[a] / YMAX * ease(p * 2.2 - 0.5 - a / MAXD * 0.5); ctx.fillRect(pt.X(a) - pt.bw / 2, pt.bot - h2, pt.bw, h2); }
      // the result for each: Ct, shown once the bars are in
      var ca = ease((p - 0.75) / 0.25); ctx.globalAlpha = ca; ctx.textBaseline = 'alphabetic'; ctx.textAlign = 'right'; ctx.font = '600 15px system-ui, sans-serif';
      panels.forEach(function (pn) { ctx.fillStyle = pn.col; ctx.fillText('Ct = ' + pn.ct.toFixed(1), pn.x1 - 4, pn.top + 20); }); ctx.globalAlpha = 1;
      var cs = data.ct.sync[model], ct = data.ct.stable[model], more = Math.round((cs / ct - 1) * 100);
      read.textContent = 'Under the ' + model.charAt(0).toUpperCase() + model.slice(1) + ' model, Ct is ' + more + '% higher when the population is entirely young (' + cs.toFixed(1) + ' against ' + ct.toFixed(1) + ')';
      cv.setAttribute('aria-label', 'Share of mosquitoes at each age. Synchronous emergence: all at ages 3 to 6 days, Ct ' + cs.toFixed(1) + '. Stable age distribution: every age present, mostly young, Ct ' + ct.toFixed(1) + '. Synchronous Ct is ' + more + ' percent higher.');
    }
    var ctl = wire(root, {canvas: cv, state: S, draw: draw, resize: resize, secs: function () { return 1.9; }, setModel: function (m) { model = m; }});
    if (reduce) S.t = 1;
    resize(); ctl.label(); draw();
  });

  // ---- The incubation race ----
  $(function () {
    var root = document.getElementById('racedemo'); if (!root) return;
    var lx; try { lx = JSON.parse(root.getAttribute('data-lx')); } catch (e) { return; }
    var cv = root.querySelector('canvas'), ctx = cv.getContext('2d'), read = root.querySelector('.cohort-read'), days = root.querySelector('.race-days'), dout = root.querySelector('.race-days-out');
    var COLS = 25, ROWS = 8, N = COLS * ROWS, model = 'logistic', n = 10, S = {t: 0, max: 11.5}, dim = null, death = [];
    function build() {
      var curve = lx[model], order = [], k; for (k = 0; k < N; k++) order.push(k);
      var r = rng(777); for (k = N - 1; k > 0; k--) { var j = Math.floor(r() * (k + 1)), tmp = order[k]; order[k] = order[j]; order[j] = tmp; }
      death = new Array(N); for (k = 0; k < N; k++) death[order[k]] = timeFor(curve, (N - k - 0.5) / N);
      S.max = n + 1.5;
    }
    function resize() { dim = sizeCanvas(cv, 250, 385); }
    function draw() {
      if (!dim) resize(); if (!dim) return;
      var W = dim.w, H = dim.h, t = S.t, curve = lx[model];
      ctx.setTransform(dim.dpr, 0, 0, dim.dpr, 0, 0); ctx.clearRect(0, 0, W, H);
      var alive = cvar('--btn', '#2b6cb0'), line = cvar('--line-strong', '#c8d0d9'), muted = cvar('--muted', '#6b7785'), txt = cvar('--text', '#1f2933'), warm = '#D55E00';
      // legend
      ctx.font = '12px system-ui, sans-serif'; ctx.textBaseline = 'middle'; ctx.textAlign = 'left';
      var cur = blend(ctx, alive, warm, Math.min(1, t / n));      // the pathogen develops inside every living mosquito, so they slowly turn orange
      var lg = [[alive, 'alive, pathogen developing', false], [warm, 'survived: can now pass it on', false], [line, 'died', true]], lgx = 12;
      lg.forEach(function (it) { ctx.beginPath(); ctx.arc(lgx + 5, 13, 4.2, 0, 6.2832); if (it[2]) { ctx.strokeStyle = it[0]; ctx.lineWidth = 1.2; ctx.stroke(); } else { ctx.fillStyle = it[0]; ctx.fill(); }
        ctx.fillStyle = muted; ctx.fillText(it[1], lgx + 15, 13); lgx += 15 + ctx.measureText(it[1]).width + 18; });
      var side = W >= 560, dw = side ? Math.round(W * 0.54) : W, dotsTop = 30, dotsH = side ? H - 62 - dotsTop : Math.min(150, dw / COLS * ROWS * 1.1);
      var sx = dw / COLS, sy = dotsH / ROWS, rad = Math.min(sx, sy) * 0.34, k, x, y;
      for (k = 0; k < N; k++) {
        x = (k % COLS + 0.5) * sx; y = dotsTop + (Math.floor(k / COLS) + 0.5) * sy; var dd = death[k], since = t - dd;
        if (dd >= n) {                       // makes it through the incubation period
          if (t < n) { ctx.globalAlpha = 1; ctx.fillStyle = cur; }
          else { var pop = Math.min(1, (t - n) / 0.8); ctx.globalAlpha = 1; ctx.fillStyle = warm; ctx.beginPath(); ctx.arc(x, y, rad * (1 + 0.45 * Math.sin(pop * Math.PI)), 0, 6.2832); ctx.fill(); continue; }
          ctx.beginPath(); ctx.arc(x, y, rad, 0, 6.2832); ctx.fill();
        } else if (t < dd) { ctx.globalAlpha = 1; ctx.fillStyle = cur; ctx.beginPath(); ctx.arc(x, y, rad, 0, 6.2832); ctx.fill(); }
        else if (since < 0.9 && !reduce) { var q = since / 0.9; ctx.globalAlpha = 1 - q; ctx.fillStyle = muted; ctx.beginPath(); ctx.arc(x, y + q * rad * 1.2, rad * (1 - 0.5 * q), 0, 6.2832); ctx.fill(); }
        else { ctx.globalAlpha = 0.35; ctx.strokeStyle = line; ctx.lineWidth = 1; ctx.beginPath(); ctx.arc(x, y, rad * 0.55, 0, 6.2832); ctx.stroke(); }
      }
      ctx.globalAlpha = 1;
      // timeline: days 0 to n, filling as the race goes; the finish line is the end of the incubation period
      var tx0 = 12, tx1 = dw - 12, ty = H - 26, p = Math.min(1, t / n);
      ctx.strokeStyle = line; ctx.lineWidth = 6; ctx.lineCap = 'round'; ctx.beginPath(); ctx.moveTo(tx0, ty); ctx.lineTo(tx1, ty); ctx.stroke();
      ctx.strokeStyle = cur; ctx.beginPath(); ctx.moveTo(tx0, ty); ctx.lineTo(tx0 + (tx1 - tx0) * p, ty); ctx.stroke(); ctx.lineCap = 'butt';
      ctx.fillStyle = muted; ctx.font = '11px system-ui, sans-serif'; ctx.textBaseline = 'top'; ctx.textAlign = 'left'; ctx.fillText('infected (day 0)', tx0, ty + 9);
      ctx.textAlign = 'right'; ctx.fillStyle = t >= n ? warm : muted; ctx.fillText('day ' + n + ': incubation period ends', tx1, ty + 9);
      // the survivorship curve (beside the dots, or under them on a narrow screen): where the race is on it, and what is left when the incubation period ends
      var cx0, cx1, cy0, cy1, dd2;
      if (side) { cx0 = dw + 40; cx1 = W - 16; cy0 = dotsTop + 8; cy1 = dotsTop + dotsH - 6; } else { cx0 = 40; cx1 = W - 16; cy0 = dotsTop + dotsH + 22; cy1 = cy0 + 78; }
      var XM = Math.max(20, n + 6), cxs = function (dv) { return cx0 + (cx1 - cx0) * dv / XM; }, cys = function (v) { return cy1 - (cy1 - cy0) * v; };
      ctx.lineWidth = 1; ctx.strokeStyle = line; ctx.beginPath(); ctx.moveTo(cx0, cy0); ctx.lineTo(cx0, cy1); ctx.lineTo(cx1, cy1); ctx.stroke();
      ctx.font = '10.5px system-ui, sans-serif'; ctx.fillStyle = muted; ctx.textAlign = 'right'; ctx.textBaseline = 'middle';
      [0, 0.5, 1].forEach(function (v) { ctx.fillText(Math.round(v * 100) + '%', cx0 - 5, cys(v)); });
      ctx.textAlign = 'center'; ctx.textBaseline = 'top'; var stp = XM <= 20 ? 5 : 10; for (dd2 = 0; dd2 <= XM; dd2 += stp) ctx.fillText(dd2, cxs(dd2), cy1 + 4);
      ctx.fillText('Age (days)', (cx0 + cx1) / 2, cy1 + 16);
      ctx.strokeStyle = line; ctx.lineWidth = 1.6; ctx.beginPath(); for (dd2 = 0; dd2 <= XM; dd2 += 0.5) { if (dd2 === 0) ctx.moveTo(cxs(dd2), cys(interp(curve, dd2))); else ctx.lineTo(cxs(dd2), cys(interp(curve, dd2))); } ctx.stroke();
      ctx.setLineDash([4, 4]); ctx.lineWidth = 1; ctx.strokeStyle = t >= n ? warm : muted; ctx.beginPath(); ctx.moveTo(cxs(n), cy0); ctx.lineTo(cxs(n), cy1); ctx.stroke(); ctx.setLineDash([]);
      var te = Math.min(t, n); ctx.strokeStyle = cur; ctx.lineWidth = 2.6; ctx.beginPath(); ctx.moveTo(cxs(0), cys(1)); for (dd2 = 0.25; dd2 < te; dd2 += 0.25) ctx.lineTo(cxs(dd2), cys(interp(curve, dd2))); ctx.lineTo(cxs(te), cys(interp(curve, te))); ctx.stroke();
      var px = cxs(te), py = cys(interp(curve, te));
      // at the end of the incubation period the point swells and glows once, like the dots that survived
      var ppop = t >= n ? Math.sin(Math.min(1, (t - n) / 0.8) * Math.PI) : 0;
      ctx.save(); ctx.shadowColor = cur; ctx.shadowBlur = 10 + 16 * ppop; ctx.fillStyle = cur; ctx.beginPath(); ctx.arc(px, py, 4 * (1 + 0.9 * ppop), 0, 6.2832); ctx.fill(); ctx.restore();
      if (t >= n) {
        ctx.setLineDash([2, 3]); ctx.lineWidth = 1; ctx.strokeStyle = warm; ctx.beginPath(); ctx.moveTo(cx0, py); ctx.lineTo(px, py); ctx.stroke(); ctx.setLineDash([]);
        var lbl = Math.round(interp(curve, n) * 100) + '% survive'; ctx.font = '600 12px system-ui, sans-serif'; ctx.fillStyle = warm; ctx.textBaseline = 'bottom';
        if (px + 12 + ctx.measureText(lbl).width > cx1) { ctx.textAlign = 'right'; ctx.fillText(lbl, px - 8, py - 6); } else { ctx.textAlign = 'left'; ctx.fillText(lbl, px + 8, py - 6); }
      }
      var f = interp(curve, Math.min(t, n)), done = t >= n;
      read.textContent = done ? Math.round(interp(curve, n) * 100) + '% make it through ' + n + ' days and can now transmit' : 'Day ' + Math.floor(t) + ' of ' + n + ' · ' + Math.round(f * 100) + '% still alive';
      cv.setAttribute('aria-label', done ? Math.round(interp(curve, n) * 100) + ' percent of mosquitoes survive the ' + n + '-day incubation period.' : 'Day ' + Math.floor(t) + ' of ' + n + ': ' + Math.round(f * 100) + ' percent alive.');
    }
    var ctl = wire(root, {canvas: cv, state: S, draw: draw, resize: resize, secs: function () { return 6.5; }, setModel: function (m) { model = m; build(); }});
    days.addEventListener('input', function () { n = Number(days.value); dout.textContent = n + ' days'; build(); if (reduce) { S.t = S.max; draw(); } else ctl.play(); });
    build(); resize(); ctl.label(); draw();
  });
})();


// ---- About tab: hover (or focus) a symbol in the equations for its meaning; click to open its assumption card ----
$(function () {
  var body = document.getElementById('eq_body'); if (!body) return;
  var DEF = {
    x: ['age in days'], m: ['mosquito density per person', 'm_dens'], a: ['biting rate: bites on humans per mosquito per day', 'a_bite'], c: ['vector competence', 'vec_comp'],
    n: ['extrinsic incubation period, in days', 'n_eip'], r: ['population growth rate', 'growth_r'], 'σ': ['age at first bite', 'first_bite'],
    p: ['daily survival probability'], 'μ': ['daily mortality hazard at this age'], l: ['survivorship: the fraction of mosquitoes still alive at this age'],
    e: ['expected days of life left at this age'], w: ['the share of the population at this age (stable age distribution)'], C: ['vectorial capacity'],
    N: ['the number of trials'], 'θ': ['one set of drawn values for every assumption'], f: ['the model: assumptions in, Ct out'], i: ['index of a trial'], k: ['a day of age (summing over ages)']
  };
  var MORT = {a: ['initial mortality hazard', 'mort_a'], b: ['rate of ageing (how fast the hazard grows with age)', 'mort_b'], s: ['deceleration of the hazard in the logistic model', 'mort_s']};
  function section(el) {                                    // which numbered paragraph ("2. Age-specific mortality") this symbol belongs to
    var pad = body.querySelector('.eq-pad') || body, node = el; while (node && node.parentNode !== pad) node = node.parentNode;
    var top = node || el;
    for (var n = top; n; n = n.previousElementSibling) { var m = (n.tagName === 'P' ? n.textContent : '').match(/^\s*(\d+)\./); if (m) return Number(m[1]); }
    return 0;
  }
  function lookup(el) {
    var sym = el.textContent.trim(); if (!sym) return null;
    if (el.closest('sub, sup') && sym.length > 1) return null;
    if ((sym === 'a' || sym === 'b' || sym === 's') && section(el) === 2) return MORT[sym];
    return DEF[sym] || null;
  }
  var tip = document.createElement('div'); tip.className = 'eq-tip'; tip.setAttribute('role', 'tooltip'); document.body.appendChild(tip);
  function show(el) {
    var d = lookup(el); if (!d) return hide();
    tip.innerHTML = '<strong>' + el.textContent.trim() + '</strong> ' + d[0] + (d[1] ? '<div class="eq-tip-go">Click to open its assumption card</div>' : '');
    tip.classList.add('on'); var r = el.getBoundingClientRect(), tw = tip.offsetWidth, th = tip.offsetHeight;
    tip.style.left = Math.max(8, Math.min(window.innerWidth - tw - 8, r.left + r.width / 2 - tw / 2)) + 'px';
    tip.style.top = (r.top - th - 8 < 8 ? r.bottom + 8 : r.top - th - 8) + 'px';
  }
  function hide() { tip.classList.remove('on'); }
  $(body).on('mouseenter', 'i', function () { show(this); }).on('mouseleave', 'i', hide);
  $(body).on('focusin', 'i', function () { show(this); }).on('focusout', 'i', hide);
  $(body).on('click keydown', 'i', function (e) {
    if (e.type === 'keydown' && e.key !== 'Enter' && e.key !== ' ') return;
    var d = lookup(this); if (!d || !d[1]) return; e.preventDefault(); hide();
    var b = $('<button type="button" class="src-link" data-card="' + d[1] + '" style="display:none"></button>').appendTo(document.body); b.trigger('click'); b.remove();
  });
  function mark() { $(body).find('i').each(function () { var d = lookup(this); $(this).toggleClass('eq-sym', !!d).attr({tabindex: d && d[1] ? 0 : null, role: d && d[1] ? 'button' : null}); }); }
  mark(); $('#eq_toggle').on('click', function () { setTimeout(mark, 50); });
});
