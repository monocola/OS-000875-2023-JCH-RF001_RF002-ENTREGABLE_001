import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import {
  faAngleDoubleLeft,
  faAngleDoubleRight,
} from '@fortawesome/free-solid-svg-icons';

@Component({
  selector: 'serv-talento-simple-pager',
  templateUrl: './simple-pager.component.html',
  styleUrls: ['./simple-pager.component.scss'],
})
export class SimplePagerComponent implements OnInit {
  @Input() totalItems: number;
  @Output() pageChanged = new EventEmitter<number>();
  index = 1;
  backIcon = faAngleDoubleLeft;
  nextIcon = faAngleDoubleRight;

  constructor() {}

  ngOnInit(): void {}

  get canBack() {
    return this.index > 1;
  }

  get canNext() {
    return this.index < this.totalItems;
  }

  back() {
    if (this.canBack) {
      this.index--;
      this.pageChanged.emit(this.index);
    }
  }

  next() {
    if (this.canNext) {
      this.index++;
      this.pageChanged.emit(this.index);
    }
  }
}
