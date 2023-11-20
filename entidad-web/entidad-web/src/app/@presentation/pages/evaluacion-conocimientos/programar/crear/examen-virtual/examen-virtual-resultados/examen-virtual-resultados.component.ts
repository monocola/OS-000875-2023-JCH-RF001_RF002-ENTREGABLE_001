import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { EvaluacionConocimientosRepository } from 'src/app/@domain/repository/evaluacion-conocimientos.repository';
import { EvaluacionConService } from '../../../../evaluacion-conocimientos.service';
import { Utils } from 'src/app/utils/utils';

@Component({
  selector: 'serv-talento-examen-virtual-resultados',
  templateUrl: './examen-virtual-resultados.component.html',
  styleUrls: ['./examen-virtual-resultados.component.scss'],
})
export class ExamenVirtualResultadosComponent implements OnInit {
  resultado: string;
  cabecera: any;
  puntajeTotal: any;
  puntajeMax: any;
  puntajeMin: any;
  fechaActual: any;

  constructor(
    public helperService: EvaluacionConService,
    private evaluacionConocimientosService: EvaluacionConocimientosRepository,
    private router: Router
  ) {}

  ngOnInit(): void {
    if (!this.helperService.formResultadoExamen)
      this.helperService.initializeForm();

    this.resultado = this.form.resultado.value;
    this.puntajeTotal = parseFloat(this.form.puntajeTotal.value).toFixed(2);
    this.puntajeMax = this.form.puntajeMax.value;
    this.puntajeMin = this.form.puntajeMin.value;
    this.fechaActual = Utils.formatFechaDate(new Date(), 'DD/MM/YYYY');
    this.cargarCabecera();
  }

  cargarCabecera() {
    this.evaluacionConocimientosService
      .examenVirtualCabecera(this.f.programacionId.value)
      .subscribe((res) => {
        this.cabecera = res;
      });
  }

  get form() {
    return this.helperService.formResultadoExamen.controls;
  }

  get f() {
    return this.helperService.formProgramaciones.controls;
  }
}
