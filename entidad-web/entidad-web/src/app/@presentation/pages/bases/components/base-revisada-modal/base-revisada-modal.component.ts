import { Component, OnInit } from '@angular/core';
import { MatDialogRef } from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-base-revisada-modal',
  templateUrl: './base-revisada-modal.component.html',
  styleUrls: ['./base-revisada-modal.component.scss'],
})
export class BaseRevisadaModalComponent implements OnInit {
  constructor(private matDialogRef: MatDialogRef<BaseRevisadaModalComponent>) {}

  ngOnInit(): void {}

  onNoClick(value = null) {
    this.matDialogRef.close(value);
  }
}
