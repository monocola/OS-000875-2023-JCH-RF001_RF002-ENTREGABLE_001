import { Component, Inject, OnInit } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-creacion-modal',
  templateUrl: './creacion-modal.component.html',
  styleUrls: ['./creacion-modal.component.scss']
})

export class CreacionModalComponent implements OnInit {

  modalidades: any[] = [];

  constructor(
    private dialogRef: MatDialogRef<CreacionModalComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any,
  ) { }

  ngOnInit(): void {
    this.modalidades = this.data;
  }

  onNoClick() {
    this.dialogRef.close();
  }

}
