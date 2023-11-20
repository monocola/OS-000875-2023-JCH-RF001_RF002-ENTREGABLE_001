import { Component, Inject, OnInit } from '@angular/core';
import {
  MatDialogRef,
  MAT_DIALOG_DATA,
} from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-modal-prevent-close',
  templateUrl: './modal-prevent-close.component.html',
  styleUrls: ['./modal-prevent-close.component.scss'],
})
export class ModalPreventCloseComponent implements OnInit {
  constructor(
    private matDialogRef: MatDialogRef<ModalPreventCloseComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {}

  ngOnInit(): void {}

  onNoClick(ans: boolean = false) {
    this.matDialogRef.close(ans);
  }
}
