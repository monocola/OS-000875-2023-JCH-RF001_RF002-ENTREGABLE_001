import { Component, Input, OnInit } from '@angular/core';
import { AbstractControl } from '@angular/forms';
import { monthsValues, yearsValues, yearsValueTwo } from 'src/app/utils/values';

@Component({
  selector: 'serv-talento-month-and-year-field',
  templateUrl: './month-and-year-field.component.html',
})
export class MonthAndYearFieldComponent implements OnInit {
  years = yearsValues;
  months = monthsValues;
  yearsValue = yearsValueTwo;

  @Input() label = 'DEFAULT_LABEL';
  @Input() monthControl: AbstractControl = null;
  @Input() yearControl: AbstractControl = null;

  constructor() { }

  ngOnInit(): void { }

  validateFields() {
    return (
      (this.yearControl.invalid && this.yearControl.touched) ||
      (this.monthControl.invalid && this.monthControl.touched)
    );
  }
}
