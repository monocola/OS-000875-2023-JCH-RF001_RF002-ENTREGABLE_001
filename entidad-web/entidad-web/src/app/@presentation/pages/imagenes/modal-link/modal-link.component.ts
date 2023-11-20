import { Component, Inject, OnDestroy, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-modal-link',
  templateUrl: './modal-link.component.html',
  styleUrls: ['./modal-link.component.scss'],
})
export class ModalLinkComponent implements OnInit, OnDestroy {
  form: FormGroup;
  url;

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<ModalLinkComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {}

  async ngOnInit() {
    this.initializeForm();
    this.f.link.setValue(this.data.urlWeb);
    this.f.img.setValue(this.data.urlImg);
    this.url = this.f.img.value;
  }

  initializeForm() {
    this.form = this.fb.group({
      link: '',
      img: '',
    });
  }

  get f() {
    return this.form.controls;
  }

  guardarLink() {
    this.onNoClick(this.f.link.value);
  }

  onNoClick(data = false) {
    this.dialogRef.close(data);
  }

  ngOnDestroy(): void {}
}
