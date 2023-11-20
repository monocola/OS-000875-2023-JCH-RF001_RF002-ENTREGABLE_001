import { Component, Input, OnInit } from '@angular/core';
import { AbstractControl, FormControl, Validators } from '@angular/forms';

@Component({
  selector: 'serv-talento-strings-hours-array-field',
  templateUrl: './strings-hours-array-field.component.html',
  styleUrls: ['./strings-hours-array-field.component.scss'],
})
export class StringsHoursArrayFieldComponent implements OnInit {
  @Input() title = 'DEFAULT_TITLE';
  @Input() addItemLabel = 'DEFAULT_ITEM_LABEL';
  @Input() placeholderItem = 'DEFAULT_PLACEHOLDER';

  @Input() items = [];
  @Input() control: AbstractControl = null;
  @Input() itemsToDelete: AbstractControl = null;
  @Input() mode = 0;

  @Input() maxItems = 10;

  constructor() {}

  ngOnInit(): void {
    this.control.valueChanges.subscribe(() => {
      this.validateItems(this.control);
    });
  }

  setMaxValue(index) {
    this.control.markAsDirty();
    if (Number(this.control.value[index].horas.value) > 100) {
      this.control.value[index].horas.patchValue('100');
    }
  }

  addItems() {
    this.control.value.push({
      nombreCurso: new FormControl('', Validators.required),
      horas: new FormControl('0'),
      id: null,
      estado: '1',
    });
    this.control.patchValue(this.control.value);
    this.validateItems(this.control);
    this.control.markAsDirty();
  }

  removeItem(i) {
    if (this.control.value[i].id) {
      this.control.value[i].estado = '0';
      this.itemsToDelete.patchValue([
        ...this.itemsToDelete.value,
        this.control.value[i],
      ]);
    }
    this.control.value.splice(i, 1);
    this.control.patchValue(this.control.value);
    this.validateItems(this.control);
    this.control.markAsDirty();
  }

  validateItems(control: AbstractControl) {
    let error = false;
    control.value.map((el) => {
      if (!el.nombreCurso.value) {
        error = true;
        el.nombreCurso.setErrors({ dataFaltante: true });
      } else {
        el.nombreCurso.setErrors(null);
      }

      if (!el.horas.value && el.horas.value !== 0) {
        error = true;
        el.horas.setErrors({ dataFaltante: true });
      } else {
        el.horas.setErrors(null);
      }
    });

    if (error) {
      control.setErrors({ dataFaltante: true });
    } else {
      control.setErrors(null);
    }
  }
}
