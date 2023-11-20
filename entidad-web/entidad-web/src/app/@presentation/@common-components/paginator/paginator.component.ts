import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';

@Component({
  selector: 'serv-talento-paginator',
  templateUrl: './paginator.component.html',
  styleUrls: ['./paginator.component.scss']
})

export class PaginatorComponent implements OnInit {

  @Input() length;
  @Input() pageSize;
  @Input() pageSizeOptions;
  @Output() page = new EventEmitter();

  constructor() { }

  ngOnInit(): void {
  }

  pageMethod(event) {
    this.page.emit(event);
  }

}
