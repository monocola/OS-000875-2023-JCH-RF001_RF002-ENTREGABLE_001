import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import {
  faAngleLeft,
  faAngleRight,
} from '@fortawesome/free-solid-svg-icons';

@Component({
  selector: 'serv-talento-simple-slider-pager',
  templateUrl: './simple-pager-slider.component.html',
  styleUrls: ['./simple-pager-slider.component.scss'],
})
export class SimplePagerSliderComponent implements OnInit {
  @Input() totalItems: number;
  @Output() pageChanged = new EventEmitter<number>();
  index = 1;
  backIcon = faAngleLeft;
  nextIcon = faAngleRight;

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
