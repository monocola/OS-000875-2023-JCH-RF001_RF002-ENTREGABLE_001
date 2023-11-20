import { EditarPreguntasComponent } from './../editar-preguntas/editar-preguntas.component';
import { ModalSubirPreguntasMasivasComponent } from './../modal-subir-preguntas-masivas/modal-subir-preguntas-masivas.component';
import { FormBuilder, FormGroup } from '@angular/forms';
import { Component, OnInit } from '@angular/core';
import { ExportExcelModel } from 'src/app/@presentation/@service/export-excel.service';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { EvaluacionConocimientosRepository } from 'src/app/@domain/repository/evaluacion-conocimientos.repository';
import { MatDialog } from '@angular/material/dialog';
import { ModalCrearExamenComponent } from '../../examenes/modal-examen/modal-examen.component';
import { ActivatedRoute, Params } from '@angular/router';
import { Sort } from '@angular/material/sort';
import { sortDataTableComponent } from 'src/app/utils/general';
import { ModalConfirmationComponent } from 'src/app/@presentation/@common-components/modal-confirmation/modal-confirmation.component';

@Component({
  selector: 'serv-talento-registro',
  templateUrl: './registro.component.html',
  styleUrls: ['./registro.component.scss'],
})
export class RegistroComponent implements OnInit {
  preguntas = [];
  preguntasColumns = [];
  activarTitulo = false;
  tituloTemporal = '';

  examenId = 0;
  sumatoriaPuntos = 0;
  myForm: FormGroup;

  constructor(
    public fb: FormBuilder,
    private toastService: ToastService,
    private evaluacionConocimientosService: EvaluacionConocimientosRepository,
    private dialog: MatDialog,
    private activatedRoute: ActivatedRoute
  ) { }

  ngOnInit(): void {
    this.initializeForm();
    this.initializeColumns();

    this.activatedRoute.params.subscribe((params: Params) => {
      if (params.idEdicion) {
        this.examenId = params.idEdicion;
        this.getDetalleExamen();
      }
    });
  }

  getDetalleExamen() {
    this.evaluacionConocimientosService
      .getDetalleExamen(this.examenId)
      .subscribe((res) => {
        this.tituloTemporal = res.nombreExamen;
        this.f.examen.setValue(res.nombreExamen);
        this.preguntas = res.lista;

        this.sumarPuntos();
      });
  }

  initializeForm() {
    this.myForm = this.fb.group({
      examen: '',
    });

  }

  get f() {
    return this.myForm.controls;
  }

  toggleTitulo() {
    this.activarTitulo = !this.activarTitulo;
  }
  cancelar() {
    this.f.examen.setValue(this.tituloTemporal);
    this.toggleTitulo();
  }

  guardarExamen() {
    if (this.f.examen.value === '') {
      this.toastService.showToast('El campo no puede estar vacio', 'warning');
      return;
    }

    this.evaluacionConocimientosService
      .guardarActualizarExamen(this.examenId, this.f.examen.value)
      .subscribe((res) => {
        this.toastService.showToast(res.mensaje, 'success');
        this.examenId = res.examenId;
        this.tituloTemporal = this.f.examen.value;
        this.toggleTitulo();
      });
  }

  initializeColumns() {
    this.preguntasColumns = [
      {
        name: '#',
        dataKey: 'examenDetalleId',
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
        width: '50%',
      },
    ];
  }

  delete(event) {
    const confirmModal = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Eliminar Pregunta',
        bodyText: '¿Está seguro de eliminar la pregunta?',
      },
    });
    confirmModal.afterClosed().subscribe((res) => {
      if (res === true) {
        this.evaluacionConocimientosService
          .eliminarExamenDetalle(event.examenDetalleId)
          .subscribe((res) => {
            this.toastService.showToast(res, 'success');
            this.getDetalleExamen();
            this.sumarPuntos();
          });
      }
    });

  }


  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de Preguntas';
    model.headers = ['#', 'CATEGORIA', 'PREGUNTA'];
    model.keys = [
      'examenDetalleId',
      'descripcionCategoria',
      'descripcionPregunta',
    ];
    return model;
  }

  openModalCrearExamen(data?: any) {
    if (this.examenId === 0) {
      this.toastService.showToast(
        'Primero debe de registrar un exámen',
        'danger',
        'Atención'
      );
    } else {
      const modalExamen = this.dialog.open(ModalCrearExamenComponent, {
        width: '56rem',
        data: {
          idExamen: this.examenId,
          isEditar: this.preguntas.length > 0 ? true : false,
          titulo: 'Seleccionar Preguntas',
          idCategoria: null,
        },
      });
      modalExamen.afterClosed().subscribe((res) => {
        if (res) {
          this.getDetalleExamen();
        }
      });
    }
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.preguntas);
  }

  sumarPuntos(): void {
    this.sumatoriaPuntos = 0;
    for (let index = 0; index < this.preguntas.length; index++) {
      this.sumatoriaPuntos += this.preguntas[index].puntajePregunta;
    }

  }

  openModalSubirPreguntasMasiva() {
    const modalPreguntas = this.dialog.open(ModalSubirPreguntasMasivasComponent, {
      width: '45rem',
      // data: {
      //   idExamen: this.examenId,
      //   isEditar: this.preguntas.length > 0 ? true : false,
      //   titulo: 'Seleccionar Preguntas',
      //   idCategoria: null,
      // },
    });
    // modalPreguntas.afterClosed().subscribe((res) => {
    //   if (res) {
    //     this.getDetalleExamen();
    //   }
    // });
  }

  editPregunta($event) {
    // abrimos modal
    console.log($event);
    let modal = this.dialog.open(EditarPreguntasComponent, {
      width: '46rem',

      data: {
        idCategoria: null,
        categoria: null,
        idPregunta: null,
        isVerDetalle: false,
        titulo: 'Editar Pregunta',
      },
    });
    modal.afterClosed().subscribe((res) => {
      if (res) {
        // this.getBandeja();
      }
    });
  }

}
