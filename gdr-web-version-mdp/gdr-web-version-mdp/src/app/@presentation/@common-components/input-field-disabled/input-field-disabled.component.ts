import { Component, EventEmitter, Input, Output } from '@angular/core';
import { AbstractControl } from '@angular/forms';

@Component({
  selector: 'input-field-disabled',
  templateUrl: './input-field-disabled.component.html',
})
export class InputFieldDisabledComponent {
  @Input() control: AbstractControl;
  @Input() maxlength = 250;
  @Input() minlength = 2;
  @Input() label = '';
  @Input() placeholder = '';

  @Input() banKeyType: number = null;

  @Input() type = 'text';
  @Input() max = null;
  @Input() min = null;
  @Input() upperCase = false;
  @Input() size = 'large';
  @Input() prefixContent = '';
  @Input() suffixContent = '';

  @Output() change = new EventEmitter();
  @Output() keyup = new EventEmitter();
  @Output() blur = new EventEmitter();

  constructor() {}

  onKeyUp() {
    if (this.banKeyType) {
      const value = this.control.value;
      this.control.patchValue(
        value.replace(regexBanInputs[this.banKeyType - 1].regex, '')
      );
      if (this.banKeyType === 7) {
        this.control.patchValue(this.control.value.toLowerCase());
      }
    }
    if (this.upperCase) {
      this.control.patchValue(this.control.value.toUpperCase());
    }
  }

  onBlur() {
    if (typeof this.control.value === 'string')
      this.control.patchValue(this.control.value.trim());
  }
}

export const regexBanInputs = [
  {
    type: '1',
    descripcion: 'Anexos o telefonos - Números mas guion y ()',
    regex: /[^0-9-() ]/g,
  },
  {
    type: '2',
    descripcion: 'Solo números, sin signos ni espacios',
    regex: /[^0-9]/g,
  },
  {
    type: '3',
    descripcion: 'Solo letras',
    regex: /[^a-zA-ZñÑáéíóúÁÉÍÓÚäÄëËïÏöÖüÜ ]/g,
  },
  {
    type: '4',
    descripcion: 'Alfanumérico',
    regex: /[^a-zA-ZñÑáéíóúÁÉÍÓÚäÄëËïÏöÖüÜ0-9 ]/g,
  },
  {
    type: '5',
    descripcion: 'Letras con algunos signos principales',
    regex: /[^a-zA-ZñÑáéíóúÁÉÍÓÚäÄëËïÏöÖüÜ0-9-., ]/g,
  },
  {
    type: '6',
    descripcion: 'Alfanumérico con .',
    regex: /[^a-zA-ZñÑáéíóúÁÉÍÓÚäÄëËïÏöÖüÜ0-9. ]/g,
  },
  {
    type: '7',
    descripcion: 'Para correos',
    regex: /[^a-zA-ZñÑáéíóúÁÉÍÓÚäÄëËïÏöÖüÜ0-9-_.@ ]/g,
  },
  {
    type: '8',
    descripcion: 'Decimales',
    regex: /[^0-9.]/g,
  },
];
