import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialogRef } from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-modal-cese',
  templateUrl: './modal-cese.component.html',
  styleUrls: ['./modal-cese.component.scss']
})
export class ModalCeseComponent implements OnInit {
  registerForm: FormGroup;

  constructor(
    private matDialog: MatDialogRef<ModalCeseComponent>,
    private fb: FormBuilder,
    ) { }

  ngOnInit(): void {
    this.initializeForm();
  }

  initializeForm() {
    this.registerForm = this.fb.group({
      fechaCese: ['', [Validators.required]]
    });

    setTimeout(() => {
      const input = document.getElementById('fechaCese');


      input.setAttribute('maxlength', '10');


      input.onkeydown = (e: any) => {
        return this.isNumeric(input, e.keyCode);
      };


      input.onkeyup = (e: any) => {
        this.validateDateFormat(input, e.keyCode);
      };
    }, 250);
  }

  onNoClick(flag: boolean = false) {
    this.matDialog.close(flag);
  }

  isShift: boolean = false;
  seperator: string = '/';
  isNumeric(input: any, keyCode: any) {
    console.log(keyCode);

    if (keyCode === 16) {
      this.isShift = true;
    }

    if (
      ((keyCode >= 48 && keyCode <= 57) ||
        keyCode === 8 ||
        keyCode === 46 ||
        keyCode === 37 ||
        keyCode === 39 ||
        (keyCode >= 96 && keyCode <= 105)) &&
      this.isShift === false
    ) {
      if (
        (input.value.length === 2 || input.value.length === 5) &&
        keyCode !== 8 && keyCode !== 46
      ) {
        input.value += this.seperator;
      }

      return true;
    } else {
      return false;
    }
  }

  validateDateFormat(input, keyCode) {
    let dateString = input.value;
    if (keyCode === 16) {
      this.isShift = false;
    }
    let regex = /(((0|1)[0-9]|2[0-9]|3[0-1])\/(0[1-9]|1[0-2])\/((19|20)\d\d))$/;

    // Check whether valid dd/MM/yyyy Date Format.
    if (regex.test(dateString) || dateString.length === 0) {
      // Es valido
    } else {
      // Es invalido
    }
  }
}
