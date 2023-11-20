import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';

@Component({
  selector: 'gme-web-paginator',
  templateUrl: './paginator.component.html',
  styleUrls: ['./paginator.component.scss']
})

export class PaginatorComponent  {

  @Input() length;
  @Input() pageSize;
  @Input() pageSizeOptions;
  @Output() page = new EventEmitter();

  pageMethod(event) {
    this.page.emit(event);
  }

}
