import { Component, Input, OnInit } from '@angular/core';
import { AbstractControl, FormControl, Validators } from '@angular/forms';

@Component({
  selector: 'serv-talento-base-auto-arrays-field',
  templateUrl: './base-auto-arrays-field.component.html',
  styleUrls: ['./base-auto-arrays-field.component.scss']
})
export class BaseAutoArraysFieldComponent implements OnInit {
  @Input() title = 'DEFAULT_TITLE';
  @Input() addItemLabel = 'DEFAULT_ITEM_LABEL';
  @Input() placeholderItem = 'DEFAULT_PLACEHOLDER';

  @Input() items = [];
  @Input() control: AbstractControl = null;
  @Input() itemsToDelete: AbstractControl = null;
  @Input() mode = 0;

  @Input() maxItems = 10;
  @Input() textClass = "''";
  @Input() getItem = [];


  constructor() { }

  ngOnInit(): void {
    console.info("this.control - cambio base-auto-arrays");
    console.info(this.control);
    console.info("this.getItem - cambio base-auto-arrays");
    console.info(this.getItem);
    this.getItemCargado(this.getItem);
    this.control.valueChanges.subscribe(() => { this.validateItems(this.control); });
  }

  /*setMaxValue(index) {
    this.control.markAsDirty();
    if (Number(this.control.value[index].horas.value) > 100) {
      this.control.value[index].horas.patchValue('100');
    }
  }*/

  addItems() {
    console.info("this.control - agrega");
    console.info(this.control);
    this.control.value.push({
      declaracionId: null,
      descripcion: new FormControl('', Validators.required),
      tipoId: null,
      estado: '1',
      isServir: '0',
    });
    this.control.patchValue(this.control.value);
    this.validateItems(this.control);
    this.control.markAsDirty();
  }

  removeItem(i) {
    console.info("removeItem - i");
    console.info(i);

    if (this.control.value[i].declaracionId) {
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

    console.info("validateItems");
    console.info(control);
    console.info(control.value);
    let error = false;
    control.value.map((el) => {
      if (!el.descripcion.value) {
        error = true;
        el.descripcion.setErrors({ dataFaltante: true });
      } else {
        el.descripcion.setErrors(null);

        console.info("VALIDAR DATOS");
        console.info(el.descripcion.value);
        if ( el.descripcion.value.maeDetalleEntidadId != null ) {
          el.tipoId =  el.descripcion.value.maeDetalleEntidadId;
        }
      }
    });

    if (error) {
      control.setErrors({ dataFaltante: true });
    } else {
      control.setErrors(null);
    }
    console.info("validateItems - salida");
    console.info(control);
  }

  getItemCargado(body) {
    console.info("this.control - getItemCargado");
    console.info(this.control);

    console.info("body - getItemCargado");
    console.info(body);

    body.value.forEach(element => {
      this.control.value.push({
        declaracionId: element.declaracionId,
        descripcion: new FormControl(element, Validators.required),
        tipoId: element.tipoId,
        estado: element.estado,
        isServir: element.isServir,
      });
    });

    console.info("this.control - getItemCargado  3");
    console.info(this.control);


  }

}
