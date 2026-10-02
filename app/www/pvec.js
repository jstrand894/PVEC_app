
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
      }
      $(document).on('click', '#run', function() {
        if (run) return;
        run = {t0: performance.now(), done: false};
        // Wait a tick so Shiny registers the click before the button is disabled
        setTimeout(function() { $('#run').addClass('running').prop('disabled', true); setRun(0); run.timer = setInterval(stepRun, 30); }, 0);
      });
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
        var imgs = $('#' + $(this).data('target') + ' .draw-tile:visible img, #' + $(this).data('target') + ' .combine-img:visible img').toArray();
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
        p.set('temp', $('#temp_delta').val()); p.set('temp.curve', $('#temp_curve').val()); p.set('temp.ref', $('#temp_ref').val()); p.set('temp.pe', $('#temp_pe').val()); p.set('temp.pm', $('#temp_pm').val()); p.set('temp.pa', $('#temp_pa').val());
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
        try { localStorage.removeItem('vc_howto'); } catch (e) {}
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
      function syncRunStale() {
        var stale = $('#stale_note .stale-note').length > 0, b = $('#run');
        if (!b.length) return;
        $('body').toggleClass('results-stale', stale);   // also lights the dots beside the result tabs amber
        b.toggleClass('stale', stale);
        if (!b.hasClass('running')) setRunLabel(stale ? 'Run new simulation' : 'Run simulation');
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
  function load(item) { Shiny.setInputValue('scenario_load', Number($(item).data('id')), {priority: 'event'}); }
  $(document).on('click', '.scen-item', function(e) {
    if ($(e.target).closest('.scen-edit, .scen-cmp, .scen-run, .scen-del, .scen-input').length) return;
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
