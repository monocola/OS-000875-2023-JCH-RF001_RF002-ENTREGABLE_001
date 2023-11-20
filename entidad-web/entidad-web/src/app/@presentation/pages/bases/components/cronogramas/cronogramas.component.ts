import { CdkDragDrop, moveItemInArray } from '@angular/cdk/drag-drop';
import { Component, Input, OnInit } from '@angular/core';
import { AbstractControl } from '@angular/forms';

@Component({
  selector: 'cronogramas',
  templateUrl: './cronogramas.component.html',
  styleUrls: ['./cronogramas.component.scss'],
})
export class CronogramasComponent implements OnInit {
  @Input() items: AbstractControl;

  @Input() orderable = true;

  canAdd = true;

  constructor() {}

  ngOnInit(): void {
    this.items.valueChanges.subscribe(() => this.validateItems());
  }

  addNewItem() {
    if (!this.canAdd) {
      this.items.markAllAsTouched();
      return;
    }
    const actualValue = this.items.value.slice(0);
    actualValue.push({
      descripcion: '',
      responsable: '',
      fechaInicio: null,
      fechaFin: null,
    });
    this.items.patchValue(actualValue);
    this.items.markAsUntouched();
  }

  removeItem(index: number) {
    const array = this.items.value.slice(0);
    array.splice(index, 1);
    this.items.patchValue(array);
    this.validateItems();
  }

  drop(event: CdkDragDrop<string[]>) {
    this.items.markAsDirty();
    moveItemInArray(this.items.value, event.previousIndex, event.currentIndex);
  }

  validateItems() {
    this.canAdd = true;
    this.items.markAsPristine();
    this.items.value.forEach((item) => {
      if (
        item.descripcion === '' ||
        item.responsable === '' ||
        item.fechaInicio === null ||
        item.fechaFin === null
      ) {
        this.canAdd = false;
        this.items.markAsDirty();
        return;
      }
    });
  }

  preventKeypress(event: KeyboardEvent) {
    event.preventDefault();
  }
}
