import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { sortDataTableComponent } from 'src/app/utils/general';
import { Sort } from '@angular/material/sort';
import { TableColumn } from '../../../@common-components/material-table/table-column';
import { EvaluacionConocimientosRepository } from 'src/app/@domain/repository/evaluacion-conocimientos.repository';
import { ModalConfirmationComponent } from '../../../@common-components/modal-confirmation/modal-confirmation.component';
import { ExportExcelModel } from '../../../@service/export-excel.service';
import { MatDialog } from '@angular/material/dialog';
import { EvaluacionConService } from '../evaluacion-conocimientos.service';
import { Router } from '@angular/router';
import { ToastService } from 'src/app/@presentation/@common-components/toast';

@Component({
  selector: 'serv-talento-programar',
  templateUrl: './programar.component.html',
  styleUrls: ['./programar.component.scss'],
})
export class ProgramarComponent implements OnInit {
  filterForm: FormGroup;
  listaEvaluacionColumns = [];
  evaluaciones: any[] = [];
  evaluacionColumns: TableColumn[];
  modalidad = [];
  convocatorias = [];
  perfiles = [];
  examenes = [];

  constructor(
    private fb: FormBuilder,
    private evaluacionConocimientosService: EvaluacionConocimientosRepository,
    private dialog: MatDialog,
    public helperService: EvaluacionConService,
    private router: Router,
    private toast: ToastService
  ) {}

  ngOnInit(): void {
    this.initializeForm();
    this.getBandejaEvaluacion();
    this.helperService.initializeValues();
    this.getModalidad();
    this.getConvocatorias();
    this.getExamenes();
  }

  changePerfil() {
    let ex = null;
    if (this.f.convocatoria.value === null) {
      ex = null;
    } else {
      ex = this.f.convocatoria.value.id;
    }

    this.f.perfilPuesto.patchValue('');
    if (ex !== null) {
      this.f.perfilPuesto.enable();
      this.f.perfilPuesto.updateValueAndValidity();
      this.evaluacionConocimientosService.comboPerfiles(ex).subscribe((res) => {
        this.perfiles = res;
      });
    } else {
      this.f.perfilPuesto.disable();
      this.f.perfilPuesto.updateValueAndValidity();
      this.perfiles = [];
    }
  }

  getModalidad() {
    this.evaluacionConocimientosService
      .listarModalidadEvaluacion()
      .subscribe((res) => {
        this.modalidad = res;
      });
  }

  getConvocatorias() {
    this.evaluacionConocimientosService
      .comboConvocatorias()
      .subscribe((res) => {
        this.convocatorias = res;
      });
  }

  getExamenes() {
    this.evaluacionConocimientosService
      .listarExamenesProgramacion()
      .subscribe((res) => {
        this.examenes = res;
      });
  }

  getBandejaEvaluacion() {
    this.evaluacionConocimientosService
      .listarProgramaciones(
        this.f.convocatoria.value === null
          ? null
          : this.f.convocatoria.value.id === 'undefined'
          ? null
          : this.f.convocatoria.value.id,
        this.f.evaluacion.value === null ? null : this.f.evaluacion.value.id,
        this.f.selModalidad.value,
        this.f.perfilPuesto.value,
        this.f.selGrupo.value
      )
      .subscribe((res) => {
        this.evaluaciones = [...res];
        this.evaluaciones.forEach((element) => {
          element.grupo = this.getGrupoLabel(element.grupo);
          element.validador =
            !element.flagCorreo && element.toSend ? true : false;
        });
      });
  }

  getGrupoLabel(grupo: number) {
    if (grupo === 0) {
      return 'SIN GRUPOS';
    } else if (grupo === 1) {
      return '1 GRUPO';
    } else {
      return grupo + ' GRUPOS';
    }
  }

  get f() {
    return this.filterForm.controls;
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.evaluaciones);
  }

  clear() {
    this.initializeForm();
    this.getBandejaEvaluacion();
  }

  initializeForm() {
    this.filterForm = this.fb.group({
      convocatoria: '',
      perfilPuesto: { value: '', disabled: true },
      evaluacion: '',
      selGrupo: '',
      selModalidad: '',
    });

    this.evaluacionColumns = [
      {
        name: 'CÓDIGO DE CONVOCATORIA',
        dataKey: 'convocatoria',
        position: 'left',
        isSortable: true,
        width: '20%',
      },

      {
        name: 'PERFIL',
        dataKey: 'perfil',
        position: 'left',
        isSortable: true,
        width: '20%',
      },

      {
        name: 'GRUPOS',
        dataKey: 'grupo',
        position: 'left',
        isSortable: true,
        width: '12%',
      },
      {
        name: 'APTOS',
        dataKey: 'aptos',
        position: 'center',
        isSortable: true,
        width: '10%',
      },
      {
        name: 'CONVOCADOS',
        dataKey: 'convocados',
        position: 'center',
        isSortable: true,
        width: '10%',
      },
      {
        name: 'MODALIDAD',
        dataKey: 'modalidad',
        position: 'center',
        isSortable: true,
        width: '15%',
      },
    ];
  }

  configEvaluacion(e) {
    this.helperService.enviarProgramacion(e);
    this.router.navigateByUrl(
      'pages/evaluacion-conocimientos/programar/grupos'
    );
  }

  removeEvaluacion(e) {
    const confirmModal = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Eliminar Programación',
        bodyText:
          'Se eliminará el perfil ' +
          e.perfil +
          (e.grupo.toLowerCase() === 'sin grupos'
            ? ''
            : ' con ' + e.grupo.toLowerCase()) +
          '. ¿Está seguro de continuar?',
      },
    });
    confirmModal.afterClosed().subscribe((res) => {
      if (res === true) {
        this.evaluacionConocimientosService
          .eliminarProgramacionByConvocatoria(e.convocatoriaId, e.perfilId)
          .subscribe((mensaje) => {
            let posicion = mensaje.indexOf('No');
            if (posicion !== -1) {
              this.toast.showToast(mensaje, 'info');
            } else {
              this.toast.showToast(mensaje, 'success', 'Atención');
            }

            this.getBandejaEvaluacion();
          });
      }
    });
  }

  enviarCorreo(e) {
    const confirmModal = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Enviar Correo',
        bodyText: '¿Esta seguro de enviar correo a los postulantes?',
      },
    });
    confirmModal.afterClosed().subscribe((res) => {
      if (res === true) {
        this.evaluacionConocimientosService
          .envioCorreo(e.convocatoriaId, e.perfilId)
          .subscribe((mensaje) => {
            this.toast.showToast(mensaje, 'success', 'Atención');

            this.getBandejaEvaluacion();
          });
      }
    });
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de programaciones';
    model.headers = [
      'CÓDIGO DE CONVOCATORIA',
      'PERFIL',
      'GRUPOS',
      'APTOS',
      'CONVOCADOS',
      'MODALIDAD',
    ];
    model.keys = [
      'convocatoria',
      'perfil',
      'grupo',
      'aptos',
      'convocados',
      'modalidad',
    ];
    return model;
  }
}
