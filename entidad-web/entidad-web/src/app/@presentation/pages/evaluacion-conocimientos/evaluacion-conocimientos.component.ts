import { Component, OnInit } from '@angular/core';
import { FormBuilder } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { EvaluacionConocimientosRepository } from 'src/app/@domain/repository/evaluacion-conocimientos.repository';
import { ModalConfirmationComponent } from '../../@common-components/modal-confirmation/modal-confirmation.component';
import { ToastService } from '../../@common-components/toast';
import { Router } from '@angular/router';
import { EvaluacionConService } from './evaluacion-conocimientos.service';

@Component({
  selector: 'serv-talento-evaluacion-conocimientos',
  templateUrl: './evaluacion-conocimientos.component.html',
  styleUrls: ['./evaluacion-conocimientos.component.scss'],
})
export class EvaluacionConocimientosComponent implements OnInit {
  categorias = [];
  examenes = [];
  evaluaciones = [];
  nombre: string = '';
  objeto: any = {};

  constructor(
    private fb: FormBuilder,
    private evaluacionConocimientosService: EvaluacionConocimientosRepository,
    private dialog: MatDialog,
    private toastService: ToastService,
    private router: Router,
    public helperService: EvaluacionConService
  ) {}

  ngOnInit(): void {
    this.getListaDeCategorias();
    this.getListaDeExamenes();
    this.getListaDeEvaluaciones();
    this.helperService.initializeForm();
  }

  getListaDeCategorias() {
    this.evaluacionConocimientosService
      .listarCategorias(3, null)
      .subscribe((res) => {
        this.categorias = res;
      });
  }

  getListaDeExamenes() {
    this.evaluacionConocimientosService
      .listarExamenes(3, null, null, null)
      .subscribe((res) => {
        this.examenes = res;
        this.examenes.forEach((element) => {
          element.cod =
            element.codigoConvocatoria.length === 0
              ? ''
              : element.codigoConvocatoria + ' | ' + element.regimen;
          element.nombrePuesto =
            element.nombrePuesto === 'Pendiente de Programación'
              ? 'Pendiente de programación'
              : element.nombrePuesto;
        });
      });
  }

  getListaDeEvaluaciones() {
    this.evaluacionConocimientosService
      .listarEvaluaciones(3)
      .subscribe((res) => {
        this.evaluaciones = res;
      });
  }

  getPreguntasLabel(preguntas: number) {
    if (preguntas === 0) {
      return 'Sin Preguntas';
    } else if (preguntas === 1) {
      return '1 Pregunta';
    } else {
      return preguntas + ' Preguntas';
    }
  }

  eliminarCategoria(categoriaId: number) {
    const confirmModal = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Eliminar Categoria',
        bodyText: '¿Está seguro de continuar?',
      },
    });
    confirmModal.afterClosed().subscribe((res) => {
      if (res === true) {
        this.evaluacionConocimientosService
          .deleteCategoria(categoriaId)
          .subscribe((mensaje) => {
            this.toastService.showToast(mensaje, 'success');
            this.getListaDeCategorias();
          });
      }
    });
  }

  eliminarExamen(examenId: number) {
    const confirmModal = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Eliminar Exámen',
        bodyText: '¿Está seguro de continuar?',
      },
    });
    confirmModal.afterClosed().subscribe((res) => {
      if (res === true) {
        this.evaluacionConocimientosService
          .deleteExamen(examenId)
          .subscribe((mensaje) => {
            this.toastService.showToast(mensaje, 'success');
            this.getListaDeExamenes();
          });
      }
    });
  }

  editarCategoria(event) {
    this.helperService.enviarCategoria(event);

    this.router.navigateByUrl(
      'pages/evaluacion-conocimientos/categorias/addcategorias'
    );
  }

  verEvaluaciones(event) {
    sessionStorage.setItem('convocatoriaId', event.convocatoriaId);

    this.router.navigateByUrl(
      '/pages/evaluacion-conocimientos/lista-evaluaciones/lista-gestion-notas'
    );
  }

  editExamen(event) {
    this.router.navigateByUrl(
      '/pages/evaluacion-conocimientos/examenes/editar-registro/' +
        event.examenId
    );
  }

  openListaCompleta() {
    this.router.navigateByUrl(
      '/pages/evaluacion-conocimientos/lista-evaluaciones'
    );
  }
}
