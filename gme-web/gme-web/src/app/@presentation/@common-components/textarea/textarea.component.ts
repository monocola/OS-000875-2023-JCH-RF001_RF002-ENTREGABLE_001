import { Component, Input, Output, EventEmitter} from '@angular/core';
import { AbstractControl } from '@angular/forms';

@Component({
  selector: 'textarea-field',
  templateUrl: './textarea.component.html',
})
export class TextareaComponent {
  @Input() control: AbstractControl = null;
  @Input() maxlength = 250;
  @Input() minlength = 2;
  @Input() label = '';
  @Input() placeholder = '';

  @Input() banKeyType: number = null;

  @Input() type = 'text';
  @Input() max = null;
  @Input() min = null;
  @Input() rows = 3;

  @Output() change = new EventEmitter();
  @Output() keyup = new EventEmitter();
  @Output() blur = new EventEmitter();

  onKeyUp() {
    if (this.banKeyType) {
      const value = this.control.value;
      this.control.patchValue(
        value.replace(regexBanInputs[this.banKeyType - 1].regex, '')
      );
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
];
