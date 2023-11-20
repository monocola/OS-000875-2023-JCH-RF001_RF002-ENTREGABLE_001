import { Component, OnInit } from '@angular/core';
import { EvaluacionConocimientosRepository } from 'src/app/@domain/repository/evaluacion-conocimientos.repository';
import { MatDialog } from '@angular/material/dialog';
import { ModalCrearPreguntaComponent } from '../../preguntas/modal-pregunta.component';
import { ToastService } from 'src/app/@presentation/@common-components/toast';

import { EvaluacionConService } from '../../evaluacion-conocimientos.service';
import { TableColumn } from 'src/app/@presentation/@common-components/material-table/table-column';
import { sortDataTableComponent } from 'src/app/utils/general';
import { Sort } from '@angular/material/sort';
import { ExportExcelModel } from 'src/app/@presentation/@service/export-excel.service';
import { ModalConfirmationComponent } from 'src/app/@presentation/@common-components/modal-confirmation/modal-confirmation.component';
import { Const } from 'src/app/@data/services/const';
import { RegistroMasivoComponent } from '../registro-masivo/registro-masivo.component';

@Component({
  selector: 'serv-talento-addcategoria',
  templateUrl: './addcategoria.component.html',
  styleUrls: ['./addcategoria.component.scss'],
})
export class CategoriaAddComponent implements OnInit {
  roles = [];
  listaCategoriaColumns = [];
  activarCategoria = false;
  id: number = null;
  categoriatemp = '';
  tipoPreguntas = [];
  preguntasColumns: TableColumn[];
  searchMode = false;
  urlMaestra = Const.API_FILE_SERVER;

  constructor(
    private evaluacionConocimientosService: EvaluacionConocimientosRepository,
    private dialog: MatDialog,
    private toast: ToastService,
    public helperService: EvaluacionConService
  ) { }

  ngOnInit(): void {
    if (!this.helperService.form) this.helperService.initializeForm();
    this.categoriatemp = this.f.categoria.value;
    this.initializeForm();
    this.getBandeja();
    this.getTipoPregunta();
  }

  get f() {
    return this.helperService.form.controls;
  }

  initializeForm() {
    this.preguntasColumns = [
      {
        name: '#',
        dataKey: 'preguntaId',
        position: 'left',
        isSortable: true,
        width: '10%',
      },

      {
        name: 'PREGUNTA',
        dataKey: 'descrcorta',
        position: 'left',
        isSortable: true,
        width: '40%',
      },
      {
        name: 'TIPO PREGUNTA',
        dataKey: 'tipoPregunta',
        position: 'center',
        isSortable: true,
        width: '20%',
      },
    ];
  }

  actionButton() {
    if (this.f.categoria.enabled) {
      this.f.categoria.disable();
      this.f.categoria.updateValueAndValidity();
    } else {
      this.f.categoria.enable();
      this.f.categoria.updateValueAndValidity();
    }
  }

  activar() {
    if (this.activarCategoria) {
      this.activarCategoria = false;
    } else {
      this.activarCategoria = true;
    }
  }
  cancelar() {
    if (this.categoriatemp === '') {
      this.f.categoria.setValue('');
    } else {
      this.f.categoria.setValue(this.categoriatemp);
    }
    this.activar();
  }

  guardarCategoria() {
    this.helperService.form.markAllAsTouched();
    if (this.helperService.form.valid) {
      this.evaluacionConocimientosService
        .guardarCategoria(this.f.idcategoria.value, this.f.categoria.value)
        .subscribe((res) => {
          if (this.f.idcategoria.value !== null) {
            this.toast.showToast('Se modificó la categoría.', 'success');
          } else {
            this.toast.showToast('Se registró la categoría.', 'success');
          }
          this.f.idcategoria.setValue(res.categoriaId);
          this.categoriatemp = res.descripcion;
        });
      this.activar();
    }
  }

  getBandeja() {
    this.evaluacionConocimientosService
      .listarPreguntas(
        this.f.idcategoria.value,
        this.f.txtDescripcion.value,
        this.f.txtTipoPregunta.value
      )
      .subscribe((res) => {
        this.helperService.preguntas = [];
        res.forEach((element) => {
          this.helperService.preguntas.push({
            preguntaId: element.preguntaId,
            descripcion: element.descripcion,
            descrcorta:
              element.descripcion.length > 100
                ? element.descripcion.substring(0, 100) + '...'
                : element.descripcion,
            tipoPregunta: element.tipoPregunta,
            img: element.img,
            flagAsignado: element.flagAsignado,
            desc1:
              element.respuestas.length >= 1
                ? element.respuestas[0].descripcion
                : '',
            resp1:
              element.respuestas.length >= 1
                ? element.respuestas[0].esCorrecto
                : '',
            desc2:
              element.respuestas.length >= 2
                ? element.respuestas[1].descripcion
                : '',
            resp2:
              element.respuestas.length >= 2
                ? element.respuestas[1].esCorrecto
                : '',
            desc3:
              element.respuestas.length >= 3
                ? element.respuestas[2].descripcion
                : '',
            resp3:
              element.respuestas.length >= 3
                ? element.respuestas[2].esCorrecto
                : '',
            desc4:
              element.respuestas.length >= 4
                ? element.respuestas[3].descripcion
                : '',
            resp4:
              element.respuestas.length >= 4
                ? element.respuestas[3].esCorrecto
                : '',
            desc5:
              element.respuestas.length >= 5
                ? element.respuestas[4].descripcion
                : '',
            resp5:
              element.respuestas.length >= 5
                ? element.respuestas[4].esCorrecto
                : '',
          });
        });

        this.helperService.preguntas.forEach((e) => {
          e.settings = {};
          if (e.flagAsignado) {
            e.settings.disableEdit = true;
            e.settings.disableDelete = true;
          }
        });

        if (this.helperService.preguntas.length !== 0) {
          this.searchMode = true;
        }
      });
  }

  getTipoPregunta() {
    this.evaluacionConocimientosService
      .listarTipoPregunta()
      .subscribe((res) => {
        console.log (res);
        this.tipoPreguntas = res;
      });
  }

  EliminarPregunta(pregunta) {
    const confirmModal = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Eliminar pregunta',
        bodyText: '¿Está seguro de continuar?',
      },
    });
    confirmModal.afterClosed().subscribe((res) => {
      if (res === true) {
        this.evaluacionConocimientosService
          .deletePreguntaDeCategoria(pregunta.preguntaId)
          .subscribe(() => {
            this.getBandeja();
            this.toast.showToast(
              'Se elimino la pregunta.',
              'success',
              'Atención'
            );

            if (this.helperService.preguntas.length === 1) {
              this.searchMode = false;
            }
          });
      }
    });
  }

  VerImagen(pregunta) {
    if (pregunta.img !== null && pregunta.img !== '') {
      window.open(
        this.urlMaestra + 'evaluacionconocimiento/pregunta/' + pregunta.img,
        '_blank'
      );
    } else {
      this.toast.showToast(
        'La pregunta seleccionada no tiene imagen.',
        'success',
        'Atención'
      );
    }
  }

  clear() {
    this.f.txtDescripcion.setValue('');
    this.f.txtTipoPregunta.setValue('');
    this.getBandeja();
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.helperService.preguntas);
  }

  openModalCrearPregunta(data?: any) {
    // Validando que la categoría se haya creado
    if (this.f.idcategoria.value === null) {
      this.toast.showToast(
        'Primero debe de crear la categoría',
        'danger',
        'Atención'
      );
    } else {
      const modalNivelEducativo = this.dialog.open(
        ModalCrearPreguntaComponent,
        {
          width: '56rem',
          data: {
            idCategoria: this.f.idcategoria.value,
            categoria: this.f.categoria.value,
            idPregunta: null,
            isVerDetalle: false,
            titulo: 'Crear Pregunta',
          },
        }
      );
      modalNivelEducativo.afterClosed().subscribe((res) => {
        if (res) {
          this.getBandeja();
        }
      });
    }
  }

  openModalEditarPregunta(data?: any) {
    const modalNivelEducativo = this.dialog.open(ModalCrearPreguntaComponent, {
      width: '56rem',
      data: {
        idCategoria: null,
        categoria: this.f.categoria.value,
        idPregunta: data.preguntaId,
        isVerDetalle: false,
        titulo: 'Editar Pregunta',
      },
    });
    modalNivelEducativo.afterClosed().subscribe((res) => {
      if (res) {
        this.getBandeja();
      }
    });
  }

  openModalDetallePregunta(data?: any) {
    this.dialog.open(ModalCrearPreguntaComponent, {
      width: '56rem',
      data: {
        idCategoria: null,
        categoria: this.f.categoria.value,
        idPregunta: data.preguntaId,
        isVerDetalle: true,
        titulo: 'Detalle Pregunta',
      },
    });
  }

  openModalRegistroMasivo() {
    if (this.f.idcategoria.value === null) {
      this.toast.showToast(
        'Primero debe de crear la categoría',
        'danger',
        'Atención'
      );
    } else {
      const modalRegistroMasivo = this.dialog.open(RegistroMasivoComponent, {
        width: '45rem',
        data: {
          nombreCategoria: this.f.categoria.value,
          idCategoria: this.f.idcategoria.value,
        },
      });
      modalRegistroMasivo.afterClosed().subscribe((res) => {
        if (res) {
          this.getBandeja();
        }
      });
    }
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista preguntas';
    model.headers = [
      '#',
      'DESCRIPCIÓN PREGUNTA',
      'TIPO DE PREGUNTA',
      'TIPO RESPUESTA 1',
      'DESCRIPCIÓN RESPUESTA 1',
      'TIPO RESPUESTA 2',
      'DESCRIPCIÓN RESPUESTA 2',
      'TIPO RESPUESTA 3',
      'DESCRIPCIÓN RESPUESTA 3',
      'TIPO RESPUESTA 4',
      'DESCRIPCIÓN RESPUESTA 4',
      'TIPO RESPUESTA 5',
      'DESCRIPCIÓN RESPUESTA 5',
    ];
    model.keys = [
      'preguntaId',
      'descripcion',
      'tipoPregunta',
      'resp1',
      'desc1',
      'resp2',
      'desc2',
      'resp3',
      'desc3',
      'resp4',
      'desc4',
      'resp5',
      'desc5',
    ];
    return model;
  }
}
