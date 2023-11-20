import { CdkDragDrop, moveItemInArray } from '@angular/cdk/drag-drop';
import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { AbstractControl } from '@angular/forms';

@Component({
  selector: 'serv-talento-strings-array-field',
  templateUrl: './strings-array-field.component.html',
  styleUrls: ['./strings-array-field.component.scss'],
})
export class StringsArrayFieldComponent implements OnInit {
  @Input() title = 'DEFAULT_TITLE';
  @Input() addItemLabel = 'DEFAULT_ITEM_LABEL';
  @Input() placeholderItem = 'DEFAULT_PLACEHOLDER';

  @Input() items: AbstractControl;
  @Input() extraItems = [];
  @Input() deletedItems = [];
  @Input() mode = 0;
  @Input() noDuplicateItems: boolean = false;

  @Input() maxItems = 10;
  @Input() minItems = 1;

  @Output() change = new EventEmitter();

  @Input() orderable = true;
  @Input() showNumbers = true;

  @Input() textClass = "''";

  itemsToShow() {
    return this.items.value;
  }

  extraItemsMap: Map<number, boolean> = new Map<number, boolean> ();

  constructor() {}

  ngOnInit(): void {
    this.items.valueChanges.subscribe(() => {
      this.validateItems(this.items);

      this.extraItemsMap.clear ();
      const arrayFunctions = this.items.value;
      arrayFunctions.map((el) => {
        if (el.extra) {
          this.extraItemsMap.set (el.extra, el.descripcion);
        }
      });
    });
  }

  addNewItem() {
    const actualValue = this.items.value.slice(0);
    actualValue.push({
      funcionDetalleId: null,
      extra: '',
      descripcion: '',
      estado: 1,
    });
    this.items.patchValue(actualValue);
    this.items.markAsDirty();
  }

  removeItem(index: number) {
    const array = this.items.value.slice(0);
    if (array[index].funcionDetalleId) {
      this.deletedItems.push(array[index]);
    }
    array.splice(index, 1);
    this.items.patchValue(array);
    this.items.markAsDirty();
  }

  clear(e, index) {
    const value = this.items.value;
    this.items.value[index].descripcion = value[index].descripcion.trim();
    this.items.patchValue(value);
  }

  drop(event: CdkDragDrop<string[]>) {
    this.items.markAsDirty();
    moveItemInArray(this.items.value, event.previousIndex, event.currentIndex);
  }

  validateItems(control: AbstractControl) {
    let error = false;
    const arrayFunctions = control.value;
    arrayFunctions.map((el) => {
      if (this.mode === 1) {
        if (!el.descripcion || !el.extra) {
          error = true;
        }
      } else {
        if (!el.descripcion) {
          error = true;
        }
      }
    });
    if (error) {
      control.setErrors({ dataFaltante: true });
    } else {
      if (control.value.length >= this.minItems) {
        control.setErrors(null);
      } else {
        control.setErrors({ pocasFilas: true });
      }
    }

    return error;
  }
}
