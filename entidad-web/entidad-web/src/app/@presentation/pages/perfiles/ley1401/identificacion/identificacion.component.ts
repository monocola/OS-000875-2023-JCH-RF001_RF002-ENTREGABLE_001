import { Component, OnInit } from '@angular/core';
import { Const } from 'src/app/@data/services/const';
import { HelperLey1401Service } from '../helperLey1401.service';

@Component({
  selector: 'serv-talento-identificacion',
  templateUrl: './identificacion.component.html',
  styleUrls: ['./identificacion.component.scss'],
})
export class IdentificacionComponent implements OnInit {
  constructor(public helper1401Service: HelperLey1401Service) { }

  ngOnInit(): void {
    this.helper1401Service.initializeForm();
  }

  changeTipoPracticas() {
    const value = this.helper1401Service.tipoPracticas.filter(
      (tp) => tp.maeDetalleId === this.f.tipoPractica.value
    )[0];

    switch (value.codProg) {
      case Const.ESTUDIANTE:
        this.f.condicion.patchValue(
          this.codeCondPracticasToId(Const.MD_PRA_PRE_PROF).maeDetalleId
        );
        break;
      case Const.EGRESADO:
        this.f.condicion.patchValue(
          this.codeCondPracticasToId(Const.MD_PRA_PROF).maeDetalleId
        );
        break;
      default:
        break;
    }
  }

  get f() {
    return this.helper1401Service.identificacionForm.controls;
  }

  codeCondPracticasToId(code) {
    return this.helper1401Service.condicionPracticas.filter(
      (f) => f.codProg === code
    )[0];
  }
}
