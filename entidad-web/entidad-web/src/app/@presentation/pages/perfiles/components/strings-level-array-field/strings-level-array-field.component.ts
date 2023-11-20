import { CdkDragDrop, moveItemInArray } from '@angular/cdk/drag-drop';
import { Component, Input, OnInit } from '@angular/core';
import { AbstractControl } from '@angular/forms';

@Component({
  selector: 'serv-talento-strings-level-array-field',
  templateUrl: './strings-level-array-field.component.html',
  styleUrls: ['./strings-level-array-field.component.scss'],
})
export class StringsLevelArrayFieldComponent implements OnInit {
  @Input() title = 'DEFAULT_TITLE';
  @Input() addItemLabel = 'DEFAULT_ITEM_LABEL';
  @Input() placeholderItem = 'DEFAULT_PLACEHOLDER';

  @Input() control: AbstractControl = null;
  @Input() itemsToDelete: AbstractControl = null;

  @Input() maxItems = 10;
  @Input() type = 0;

  constructor() {}

  ngOnInit(): void {
    this.control.valueChanges.subscribe(() => this.validateItems(this.control));
  }

  addItem() {
    this.control.value.push({ nombre: '', nivel: '1' });
    this.control.patchValue(this.control.value);
    this.control.markAsDirty();
  }

  addFirstItem() {
    this.control.patchValue(this.control.value);
    if (this.type === 1) {
      this.control.value.push({ nombre: 'PROCESADOR DE TEXTOS', nivel: '1' });
      this.control.value.push({ nombre: 'HOJAS DE CÁLCULO', nivel: '1' });
      this.control.value.push({
        nombre: 'PROGRAMA DE PRESENTACIÓN',
        nivel: '1',
      });
    } else {
      this.control.value.push({ nombre: '', nivel: '1' });
    }
  }

  removeItem(index) {
    if (this.control.value[index].id) {
      this.control.value[index].estado = '0';
      this.itemsToDelete.patchValue([
        ...this.itemsToDelete.value,
        this.control.value[index],
      ]);
    }
    this.control.value.splice(index, 1);
    this.control.patchValue(this.control.value);
    this.control.markAsDirty();
  }

  clear(index) {
    const value = this.control.value;
    value[index] = {
      ...value[index],
      nombre: value[index].nombre.trim(),
    };
    this.control.patchValue(value);
  }

  drop(event: CdkDragDrop<string[]>) {
    moveItemInArray(
      this.control.value,
      event.previousIndex,
      event.currentIndex
    );
  }

  validateItems(control: AbstractControl) {
    let error = false;
    const arrayFunctions = control.value;
    arrayFunctions.map((el) => {
      if (!el.nombre) {
        error = true;
      }
    });
    error ? control.setErrors({ dataFaltante: true }) : control.setErrors(null);
  }
}
