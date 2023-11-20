import { Component, Inject, OnInit } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';

import {
  FormGroup,
  FormControl,
  Validators,
} from '@angular/forms';

@Component({
  selector: 'serv-talento-modal-alert-confirm',
  templateUrl: './modal-alert-confirm.component.html',
  styleUrls: ['./modal-alert-confirm.component.scss'],
})
export class ModalAlertConfirmComponent implements OnInit {
  comentario: string = '';
  form: FormGroup;
  constructor(
    private matDialogRef: MatDialogRef<ModalAlertConfirmComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) { }

  ngOnInit(): void {
    this.form = new FormGroup({
      comentario: new FormControl('', [Validators.required, Validators.maxLength(200)])
    });
  }

  onNoClick(ans: boolean = false) {

    if (this.data.type === 'input' && ans ) {
      this.matDialogRef.close({ response: this.form.valid, data: this.form.value.comentario });
    } else {
      this.matDialogRef.close({ response: ans });
    }
  }


  get f() {
    return this.form.value;
  }
}
