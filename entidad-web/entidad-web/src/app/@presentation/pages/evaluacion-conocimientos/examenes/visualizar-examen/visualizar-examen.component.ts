import { TableColumn } from 'src/app/@presentation/@common-components/material-table/table-column';
import { ExportExcelModel } from './../../../../@service/export-excel.service';
import { EvaluacionConocimientosRepository } from 'src/app/@domain/repository/evaluacion-conocimientos.repository';
import { FormBuilder, FormGroup } from '@angular/forms';
import { Router, ActivatedRoute, Params } from '@angular/router';
import { Component, OnInit } from '@angular/core';

@Component({
  selector: 'serv-talento-visualizar-examen',
  templateUrl: './visualizar-examen.component.html',
  styleUrls: ['./visualizar-examen.component.scss'],
})
export class VisualizarExamenComponent implements OnInit {
  preguntas = [];
  myForm: FormGroup;
  activarTitulo = false;
  sumatoriaPuntos = 0;
  tituloTemporal = '';
  examen: any;
  preguntasColumns: TableColumn[];

  constructor(
    public fb: FormBuilder,
    private router: Router,
    private evaluacionConocimientosService: EvaluacionConocimientosRepository,
    private activatedRoute: ActivatedRoute
  ) {}

  ngOnInit(): void {
    this.initializeForm();
    this.initializeColumns();
    this.examen = JSON.parse(sessionStorage.getItem('examen'));

    // this.activatedRoute.params.subscribe((params: Params) => {
    //   if (params.idEdicion) {
    this.getDetalleExamen();
    //   }
    // });
  }

  get f() {
    return this.myForm.controls;
  }

  back() {
    this.router.navigateByUrl('pages/evaluacion-conocimientos/examenes');
  }

  initializeForm() {
    this.myForm = this.fb.group({
      examen: '',
    });
  }

  toggleTitulo() {
    this.activarTitulo = !this.activarTitulo;
  }

  cancelar() {
    this.f.examen.setValue(this.tituloTemporal);
    this.toggleTitulo();
  }

  getDetalleExamen() {
    this.evaluacionConocimientosService
      .getDetalleExamen(this.examen.examenId)
      .subscribe((res) => {
        this.tituloTemporal = res.nombreExamen;
        this.f.examen.setValue(res.nombreExamen);
        this.preguntas = res.lista;
        this.preguntas.forEach((element, index) => {
          element.id = index+1;
        });
        console.log(this.preguntas);
        this.sumarPuntos();
      });
  }

  sumarPuntos(): void {
    this.sumatoriaPuntos = 0;
    for (let index = 0; index < this.preguntas.length; index++) {
      this.sumatoriaPuntos += this.preguntas[index].puntajePregunta;
    }
  }

  initializeColumns() {
    this.preguntasColumns = [
      {
        name: '#',
        dataKey: 'id',
        position: 'left',
        isSortable: true,
        width: '10%',
      },
      {
        name: 'CATEGORIA',
        dataKey: 'descripcionCategoria',
        position: 'left',
        isSortable: true,
        width: '30%',
      },
      {
        name: 'PREGUNTA',
        dataKey: 'descripcionPregunta',
        position: 'left',
        isSortable: true,
        width: '40%',
      },
      {
        name: 'PUNTOS',
        dataKey: 'puntajePregunta',
        position: 'center',
        isSortable: true,
        width: '10%',
      }
    ];
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de Preguntas';
    model.headers = ['#', 'CATEGORIA', 'PREGUNTA','PUNTOS'];
    model.keys = [
      'examenDetalleId',
      'descripcionCategoria',
      'descripcionPregunta',
      'puntajePregunta',
    ];
    return model;
  }
}
