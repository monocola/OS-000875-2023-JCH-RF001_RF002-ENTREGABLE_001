import { Component, Inject, OnInit } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-registro-masivo-error-modal',
  templateUrl: './registro-masivo-error-modal.component.html',
  styleUrls: ['./registro-masivo-error-modal.component.scss'],
})
export class RegistroMasivoErrorModalComponent implements OnInit {
  errors: any [] = [];
  constructor(
    private dialogRef: MatDialogRef<RegistroMasivoErrorModalComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {}

  ngOnInit(): void {
    this.errors = this.formatData (this.data.errors);
  }
  
  onNoClick () {
    this.dialogRef.close(0);
  }

  formatData (list: string []) {
    let returned: any [] = [];
    
    list.forEach ((error: string) => {
      if (error.split (';').length > 1) {
        let fila: string = error.split (':') [1].split (';') [0];
        let columna: string = error.split (':') [1].split (';') [1];
        let descripcion: string = error.split (':') [1].split (';') [2];

        returned.push ({
          fila: fila.split (' ') [1],
          columna: columna.split (' ') [1],
          descripcion: descripcion
        });
      }
    });

    return returned;
  }
}
