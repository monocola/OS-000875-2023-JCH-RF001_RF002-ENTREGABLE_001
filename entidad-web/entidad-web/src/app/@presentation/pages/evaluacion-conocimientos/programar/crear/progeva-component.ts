import { Component, OnInit } from '@angular/core';
import { EvaluacionConocimientosRepository } from 'src/app/@domain/repository/evaluacion-conocimientos.repository';
import { FormBuilder, FormGroup } from '@angular/forms';
import { EvaluacionConService } from '../../evaluacion-conocimientos.service';
import { TableColumn } from 'src/app/@presentation/@common-components/material-table/table-column';
import { ExportExcelModel } from 'src/app/@presentation/@service/export-excel.service';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { Router } from '@angular/router';
import { MatDialog } from '@angular/material/dialog';
import { ModalConfirmationComponent } from 'src/app/@presentation/@common-components/modal-confirmation/modal-confirmation.component';
import { FileVisualizerComponent } from 'src/app/@presentation/@common-components/file-visualizer/file-visualizer.component';
import { ModalCrearGrupoComponent } from '../../programar/modal-crear-grupo/modal-crear-grupo.component';

@Component({
  selector: 'serv-talento-progeva',
  templateUrl: './progeva-component.html',
  styleUrls: ['./progeva-component.scss'],
})
export class ProgEvaluacionComponent implements OnInit {
  evaluaciones: any[] = [];
  examenes = [];
  nombreperfil: any;
  aptos: any;
  grupo: any;
  convocados: any;
  grupos: any[];
  grupoColumns: TableColumn[];
  searchMode = false;
  totalPostulanteDisponibles: number = 0;

  filterForm: FormGroup;

  isCrearDisable: boolean;

  constructor(
    private fb: FormBuilder,
    private evaluacionConocimientosService: EvaluacionConocimientosRepository,
    public helperService: EvaluacionConService,
    private toast: ToastService,
    private dialog: MatDialog,
    private router: Router
  ) {}

  ngOnInit(): void {
    if (!this.helperService.formProgramaciones)
      this.helperService.initializeForm();
    this.initializeForm();
    this.getListaDeExamenes();

    this.getDatosPerfil();
  }

  getBandejaEvaluacion() {
    this.evaluacionConocimientosService
      .buscarBandejaCategoria(null, null)
      .subscribe((res) => {
        this.evaluaciones = [...res];
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

  crearGrupo() {
    const modalCrearGrupo = this.dialog.open(ModalCrearGrupoComponent, {
      width: '56rem',
      data: {
        nombreperfil: this.nombreperfil,
        perfilId: this.f.idPerfil.value,
        cantidadPostulantes: this.aptos,
        cantGrupos: this.grupo,
        idConvocatoria: this.f.idConvocatoria.value,
        isEditar: false,
        totalPostulantesConvocados: this.convocados,
        titulo: 'Crear grupos',
      },
    });
    modalCrearGrupo.afterClosed().subscribe((res) => {
      if (res) {
        this.getDatosPerfil();
      }
    });
  }

  verExamenVirtual(e) {
    this.helperService.enviarProgramacionId(e);
    this.router.navigateByUrl(
      'pages/evaluacion-conocimientos/programar/grupos/examen-virtual'
    );
  }
  verExamenPresencial(e) {
    this.showExamenPDF(e.programacionId);
  }
  editarGrupo(element) {
    const modalCrearGrupo = this.dialog.open(ModalCrearGrupoComponent, {
      width: '56rem',
      data: {
        nombreperfil: this.nombreperfil,
        perfilId: this.f.idPerfil.value,
        cantidadPostulantes: this.aptos,
        cantGrupos: this.grupo,
        idConvocatoria: this.f.idConvocatoria.value,
        isEditar: true,
        programacionData: element,
        totalPostulantesConvocados: this.convocados,
        titulo: 'Editar grupos',
      },
    });
    modalCrearGrupo.afterClosed().subscribe((res) => {
      if (res) {
        this.getDatosPerfil();
      }
    });
  }

  eliminarGrupo(e) {
    const confirmModal = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Eliminar Grupo',
        bodyText: '¿Está seguro de continuar?',
      },
    });
    confirmModal.afterClosed().subscribe((res) => {
      if (res === true) {
        this.evaluacionConocimientosService
          .eliminarGrupo(e.programacionId)
          .subscribe((mensaje) => {
            this.toast.showToast(mensaje, 'success', 'Atención');
            if (this.grupos.length === 1) {
              this.searchMode = false;
            }
            this.getDatosPerfil();
          });
      }
    });
  }

  get f() {
    return this.helperService.formProgramaciones.controls;
  }
  getDatosPerfil() {
    this.evaluacionConocimientosService
      .listarGruposByConvocatoriaPerfil(
        this.f.idConvocatoria.value,
        this.f.idPerfil.value
      )
      .subscribe((res) => {
        let valor = res.lista?.length;

        if (valor !== 0) {
          let result = res.lista[0];

          this.nombreperfil = result.perfil;
          this.aptos = result.aptos;
          this.grupo = result.grupos;
          this.convocados = result.convocados;
          this.grupos = [];

          if (this.aptos <= this.convocados) {
            this.isCrearDisable = true;
          } else {
            this.isCrearDisable = false;
          }

          result.listaProgramacion.forEach((element) => {
            this.grupos.push({
              programacionId: element.programacionId,
              nombreGrupo: element.nombreGrupo,
              cantidad: element.cantidad,
              fechaInicioExamen: element.fechaInicioExamen,
              fechaFinExamen: element.fechaFinExamen,
              sedeEvaluacionesId: element.sedeEvaluacionesId,
              modalidadId: element.modalidadId,
              modalidad: element.modalidad,
              evaluacion: element.evaluacion,
              evaluacionId: element.evaluacionId,
              evaluadorId: element.evaluadorId,
              referencia: element.referencia,
              indicaciones: element.indicaciones,
              link: element.link,
              periodo:
                element.fechaInicioExamen + '  ' + element.fechaFinExamen,
            });
          });
          if (this.grupos.length !== 0) {
            this.searchMode = true;
          }
        }
      });
  }
  delay(ms: number) {
    return new Promise((resolve) => setTimeout(resolve, ms));
  }
  back() {
    if (this.aptos > this.convocados) {
      this.toast.showToast(
        'Los convocados no coinciden con los aptos, por favor ingrese más grupos.',
        'warning'
      );

      (async () => {
        await this.delay(3000);

        this.router.navigateByUrl('pages/evaluacion-conocimientos/programar');
      })();
    } else {
      this.router.navigateByUrl('pages/evaluacion-conocimientos/programar');
    }
  }
  initializeForm() {
    this.grupoColumns = [
      {
        name: 'NOMBRE GRUPO',
        dataKey: 'nombreGrupo',
        position: 'left',
        isSortable: true,
        width: '20%',
      },

      {
        name: 'CANTIDAD',
        dataKey: 'cantidad',
        position: 'left',
        isSortable: true,
        width: '10%',
      },
      {
        name: 'FECHA Y HORA',
        dataKey: 'periodo',
        position: 'center',
        isSortable: true,
        width: '15%',
      },
      {
        name: 'EVALUACION',
        dataKey: 'evaluacion',
        position: 'center',
        isSortable: true,
        width: '20%',
      },
      {
        name: 'MODALIDAD',
        dataKey: 'modalidad',
        position: 'center',
        isSortable: true,
        width: '10%',
      },
    ];
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de Grupos';
    model.headers = [
      'NOMBRE GRUPO',
      'CANTIDAD',
      'FECHA Y HORA',
      'EVALUACION',
      'MODALIDAD',
    ];
    model.keys = [
      'nombreGrupo',
      'cantidad',
      'periodo',
      'evaluacion',
      'modalidad',
    ];
    return model;
  }

  showExamenPDF(idProgramacion) {
    this.evaluacionConocimientosService
      .generarExamenPdf(idProgramacion)
      .subscribe((res) => {
        if (res.status.success) {
          const base64Data = res.payload.pdfByte;
          const nombreExamen = res.payload.nombreExamen;
          this.dialog.open(FileVisualizerComponent, {
            data: {
              base64String: base64Data,
              filename: nombreExamen,
              extension: 'pdf',
            },
          });
        } else {
          this.toast.showToast(
            res.status.error.messages[0],
            'danger',
            'Atención'
          );
        }
      });
  }
}
