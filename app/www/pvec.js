
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
  Shiny.addCustomMessageHandler('hoverBars', function (m) { bars[m.chart] = m.bars || []; $('.tip-cell').each(function () { hide(this); }); });
  $(document).on('shiny:value', function (e) {
    if (e.name === 'forecast_plot' || e.name === 'sens_plot') maps[e.name] = e.value && e.value.coordmap;
  });
  function parts(cell) {
    var hl = cell.querySelector('.bar-hl'), tip = cell.querySelector('.js-tip');
    if (!hl) { hl = document.createElement('div'); hl.className = 'bar-hl'; cell.appendChild(hl); }
    if (!tip) { tip = document.createElement('div'); tip.className = 'surv-tip js-tip'; cell.appendChild(tip); }
    return {hl: hl, tip: tip};
  }
  function hide(cell) {
    var hl = cell.querySelector('.bar-hl'), tip = cell.querySelector('.js-tip');
    if (hl) hl.classList.remove('on'); if (tip) tip.style.display = 'none';
  }
  $(document).on('mousemove', '.tip-cell', function (e) {
    var cell = this, out = cell.querySelector('.shiny-plot-output'), img = out && out.querySelector('img');
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
    p.hl.classList.add('on');
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
