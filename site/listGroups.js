function genTabListGroup(opts) {
  opts = { id: '', ...opts };
  return genHTML(`
    <div class="row" ${opts.id ? `id="${opts.id}"` : ''}>
      <div class="col-4">
        <div class="list-group" role="tablist">
        </div>
      </div>
      <div class="col-8">
        <div class="tab-content">
        </div>
      </div>
    </div>`
  );
}

function genTabListItem(opts) {
  opts = { id: '', active: false, target: '', value: '', ...opts };
  return genHTML(`
    <a class="list-group-item list-group-item-action ${opts.active ? 'active' : ''}"
      ${opts.id ? `id="${opts.id}"` : ''}
      data-bs-toggle="list"
      href="#${opts.target}"
      aria-controls="${opts.target}"
      role="tab">
      ${opts.value}
    </a>`
  );
}

function genTabListContent(opts) {
  opts = { id: '', active: false, target: '', value: '', ...opts };
  return genHTML(`
    <div class="tab-pane fade ${opts.active ? 'show active' : ''}"
      ${opts.id ? `id="${opts.id}"` : ''}
      aria-labelledby="${opts.target}"
      role="tabpanel">
      ${opts.value}
    </div>`
  );
}
