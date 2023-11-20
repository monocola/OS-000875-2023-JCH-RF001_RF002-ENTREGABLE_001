import { Component, Inject, OnInit } from '@angular/core';
import { FormControl, Validators } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { DetalleMaestra } from 'src/app/@data/model/detalleMaestra';
import { Const } from 'src/app/@data/services/const';
import { HelperPerfilesService } from '../helperPerfiles.service';

@Component({
  selector: 'serv-talento-duplicar-modal',
  templateUrl: './duplicar-modal.component.html',
  styleUrls: ['./duplicar-modal.component.scss'],
  providers: [HelperPerfilesService],
})
export class DuplicarModalComponent implements OnInit {
  control = new FormControl('', Validators.required);
  items: DetalleMaestra[] = [];

  constructor(
    private dialogRef: MatDialogRef<DuplicarModalComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {}

  ngOnInit(): void {
    if (this.data.perfil.codProg === Const.MD_DL1041) {
      this.items = this.data.regimenes.filter(
        (e) => e.codProg === Const.MD_DL1041
      );
      this.control.disable();
    } else {
      this.items = this.data.regimenes.filter(
        (e) => e.codProg !== Const.MD_DL1041
      );
    }
    this.control.patchValue(
      this.data.regimenes.find((r) => r.codProg === this.data.perfil.codProg)
    );
  }

  onNoClick(dataToSend: any = false) {
    if (dataToSend) {
      dataToSend.perfilId = this.data.perfil.perfilId;
    }
    this.dialogRef.close(dataToSend);
  }
}
