import { Component, OnInit } from '@angular/core';
import { FormBuilder } from '@angular/forms';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { EvaluacionConService } from 'src/app/@presentation/pages/evaluacion-conocimientos/evaluacion-conocimientos.service';
import { EvaluacionConocimientosRepository } from 'src/app/@domain/repository/evaluacion-conocimientos.repository';
import { ActivatedRoute, Router } from '@angular/router';

@Component({
  selector: 'serv-talento-examen-virtual-cabecera',
  templateUrl: './examen-virtual-cabecera.component.html',
  styleUrls: ['./examen-virtual-cabecera.component.scss'],
})
export class ExamenVirtualCabeceraComponent implements OnInit {
  evaluaciones: any[] = [];
  cabecera: any;
  programacionId = 0;

  constructor(
    private toast: ToastService,
    public helperService: EvaluacionConService,
    private fb: FormBuilder,
    private evaluacionConocimientosService: EvaluacionConocimientosRepository,
    private activatedRoute: ActivatedRoute,
    private router: Router
  ) {}
  ngOnInit(): void {
    if (!this.helperService.formProgramaciones)
      this.helperService.initializeForm();
    this.cargarCabecera();
  }

  configEvaluacion(e) {
    this.helperService.enviarProgramacion(e);
  }

  cargarCabecera() {
    this.evaluacionConocimientosService
      .examenVirtualCabecera(this.f.programacionId.value)
      .subscribe((res) => {
        this.cabecera = res;
      });
  }

  empezarExamen() {
    this.helperService.enviarDatosExamen(
      this.cabecera.evaluacion,
      this.cabecera.evaluacionId
    );
    this.router.navigateByUrl(
      'pages/evaluacion-conocimientos/programar/grupos/examen-virtual/preguntas'
    );
  }

  get f() {
    return this.helperService.formProgramaciones.controls;
  }
}
