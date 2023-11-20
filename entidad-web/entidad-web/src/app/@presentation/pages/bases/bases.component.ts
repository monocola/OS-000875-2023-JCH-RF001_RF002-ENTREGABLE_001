import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { Sort } from '@angular/material/sort';
import { Router } from '@angular/router';
import { forkJoin } from 'rxjs';
import { Const } from 'src/app/@data/services/const';
import { EvaluacionesServirService } from 'src/app/@data/services/evaluaciones-servir.service';
import { MaestraService } from 'src/app/@data/services/maestra.service';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { BasesRepository } from 'src/app/@domain/repository/bases.repository';
import { CuentaEntidadRepository } from 'src/app/@domain/repository/cuenta.entidad.repository';
import { sortDataTableComponent } from 'src/app/utils/general';
import { TableColumn } from '../../@common-components/material-table/table-column';
import { ModalConfirmationComponent } from '../../@common-components/modal-confirmation/modal-confirmation.component';
import { ExportExcelModel } from '../../@service/export-excel.service';
import { CreacionBaseService } from './creacion-base/creacion-base.service';
import { ToastService } from '../../@common-components/toast';
import { ModalNotificacionComponent } from './components/modal-notificacion/modal-notificacion.component';
import { FileVisualizerComponent } from '../../@common-components/file-visualizer/file-visualizer.component';

@Component({
  selector: 'serv-talento-bases',
  templateUrl: './bases.component.html',
  styleUrls: ['./bases.component.scss'],
})
export class BasesComponent implements OnInit {
  // Combos
  roles = [];
  responsables = [];
  estados = [];
  regimenes = [];
  modalidades = [];
  tipos = [];
  tipoPracticas = [];
  condicionesTrabajo = [];

  bases: any[] = [];
  basesColumns: TableColumn[];

  searchMode = false;

  filterForm: FormGroup;
  const = Const;
  actualUser = this.authService.getCurrentUserValue;

  totalBasesPorRevisar: number = null;
  rangePickerStatus: string = 'basic';

  constructor(
    private fb: FormBuilder,
    private evaluacionServirService: EvaluacionesServirService,
    private maestraService: MaestraService,
    private basesService: BasesRepository,
    private router: Router,
    private helperService: CreacionBaseService,
    private cuentaAsociadaService: CuentaEntidadRepository,
    private authenticationRepository: AuthenticationRepository,
    private authService: AuthenticationRepository,
    private dialog: MatDialog,
    private toastService: ToastService
  ) {}

  ngOnInit(): void {
    this.initializeForm();
    this.initializeColumns();
    this.loadCombox();
    this.getBases();
    this.helperService.initializeValues();
  }

  get f() {
    return this.filterForm.controls;
  }

  loadCombox() {
    const getEtapasRegistro = this.maestraService.getMaestraDetalleByCod(
      'TIP_ETA_RE'
    );
    const getRegimenes = this.evaluacionServirService.getRegimenesServir(
      'TBL_REGIMEN'
    );
    const getModalidades = this.evaluacionServirService.getModalidadesServir(
      'TBL_MODALIDAD'
    );
    const getTipos = this.evaluacionServirService.getTiposServir('TBL_TIPO');
    const getTipoPracticas = this.maestraService.getMaestraDetalleByCod(
      'TBL_PER_CON_PRA'
    );
    const getRoles = this.cuentaAsociadaService.getAllRoles();
    const getCondicionesTrabajo = this.maestraService.getMaestraDetalleByCod(
      'TIP_CON_TRA'
    );

    forkJoin([
      getEtapasRegistro,
      getRegimenes,
      getModalidades,
      getTipos,
      getTipoPracticas,
      getRoles,
      getCondicionesTrabajo,
    ]).subscribe((results) => {
      this.estados = results[0];
      this.regimenes = results[1];
      this.modalidades = results[2];
      this.tipos = results[3];
      this.tipoPracticas = results[4];
      this.roles = this.setRoles(results[5]);
      this.condicionesTrabajo = results[6];
    });
  }

  setRoles(roles: any[]) {
    return roles.filter(
      (rol) =>
        rol.rolId === Const.R_COORDINADOR || rol.rolId === Const.R_GESTOR_ORH
    );
  }

  clear() {
    this.initializeForm();
    this.getBases();
  }

  getBases() {
    this.basesService
      .getBases(this.filterForm.getRawValue())
      .subscribe((res) => {
        this.bases = [...res];
        this.bases.sort((a, b) =>
          a.baseId > b.baseId ? -1 : b.baseId > a.baseId ? 1 : 0
        );
        if (
          this.totalBasesPorRevisar === null &&
          this.authenticationRepository.getCurrentUserValue.rolId ===
            this.const.R_COORDINADOR
        ) {
          let codigoPorRevisar = this.const.ETA_BASE_POR_REVISAR;
          let basesFiltradads = this.bases.filter((item) => {
            return item.codigoEtapa === codigoPorRevisar;
          });
          this.totalBasesPorRevisar = basesFiltradads.length;

          this.dialog.open(ModalNotificacionComponent, {
            data: this.totalBasesPorRevisar,
            width: '500px',
          });
        }
      });
    this.searchMode = true;
  }

  initializeForm() {
    this.filterForm = this.fb.group({
      codigo: '',
      nombrePerfil: '',
      rol: '',
      responsable: { value: '', disabled: true },
      estado: '',
      fecha: '',
      regimen: '',
      modalidad: '',
      tipo: '',
      tipoPractica: '',
      condicion: '',
    });
  }

  changeRol() {
    this.f.responsable.patchValue('');
    if (this.f.rol.value) {
      this.f.responsable.enable();
      this.f.responsable.updateValueAndValidity();
      this.cuentaAsociadaService
        .getPersonasPorRoles(this.f.rol.value)
        .subscribe((res) => {
          this.responsables = [...res];
        });
    } else {
      this.f.responsable.disable();
      this.f.responsable.updateValueAndValidity();
      this.responsables = [];
    }
  }

  revisarObs(base) {
    this.helperService.getObservaciones(base.baseId).subscribe((res: any[]) => {
      this.actionClick(base);
      this.mapObs(base, res);
    });
  }
  actionShowPDF(base) {
    let doc =
      base.codigoConvocatoria === null
        ? 'N° XXXX-XXXX-MUN'
        : base.codigoConvocatoria;
    let documento =
      'BASE ' + doc.substring(doc.indexOf('N°'), doc.lastIndexOf('-'));
    this.basesService.obtenerPdf(base.baseId).subscribe((dataPDF) => {
      this.dialog.open(FileVisualizerComponent, {
        data: {
          base64String: dataPDF,
          filename: documento,
          extension: 'pdf',
        },
      });
    }, error => {
      this.toastService.showToast(
        JSON.stringify (error),
        'danger'
      );
    });
  }

  comomnClickAction(base) {
    let regimen = null;
    let modalidad = null;
    let tipo = null;
    regimen = this.regimenes.find((e) => e.codProg === base.codigoRegimen);
    if (base.codigoModalidad) {
      modalidad = this.modalidades.find(
        (e) => e.codProg === base.codigoModalidad
      );
    }
    if (base.codigoTipo) {
      tipo = this.tipos.find((e) => e.codProg === base.codigoTipo);
    }
    this.helperService.initializeValues();
    this.helperService.idStep1 = base.baseId; // corregir poor base.id
    this.helperService.idBase = base.baseId; // corregir poor base.id
    this.helperService.jerarquiaSelected = {
      regimen,
      modalidad,
      tipo,
    };
    sessionStorage.setItem(
      'jerarquiaSelected',
      JSON.stringify(this.helperService.jerarquiaSelected)
    );
    this.helperService.jerarquiaSelected.regimen.codProg === Const.MD_DL1041
      ? (this.helperService.jerarquiaMode = 1)
      : (this.helperService.jerarquiaMode = 0);
    this.helperService.estadoBase = base.codigoEtapa;
    this.helperService.baseSeleccionada = base;
  }

  private validatedisableAll() {
    return (
      (this.authService.getCurrentUserValue.rolId ===
        this.const.R_COORDINADOR &&
        (this.helperService.estadoBase === this.const.ETA_BASE_OBSERVADO ||
          this.helperService.estadoBase === this.const.ETA_BASE_PROCESO ||
          this.helperService.estadoBase === this.const.ETA_BASE_POR_PUBLICAR ||
          this.helperService.estadoBase === this.const.ETA_BASE_PUBLICADO ||
          this.helperService.estadoBase === this.const.ETA_BASE_POR_REVISAR ||
          this.helperService.estadoBase === this.const.ETA_BASE_REVISADO ||
          this.helperService.estadoBase ===
            this.const.ETA_BASE_POR_PUBLICAR)) ||
      (this.authService.getCurrentUserValue.rolId === this.const.R_GESTOR_ORH &&
        (this.helperService.estadoBase === this.const.ETA_BASE_POR_PUBLICAR ||
          this.helperService.estadoBase === this.const.ETA_BASE_POR_REVISAR ||
          this.helperService.estadoBase === this.const.ETA_BASE_PUBLICADO ||
          this.helperService.estadoBase === this.const.ETA_BASE_REVISADO))
    );
  }

  actionClick(base) {
    this.comomnClickAction(base);
    if (this.validatedisableAll()) {
      this.helperService.disableAllFields = true;
    }
    this.router.navigateByUrl('pages/gestionbases/creacion');
  }

  showClick(base) {
    this.comomnClickAction(base);
    if (this.validatedisableAll()) {
      this.helperService.disableAllFields = true;
      this.helperService
        .getObservaciones(base.baseId)
        .subscribe(async (res: any[]) => {
          this.mapObs(base, res);
        });
    }
    this.router.navigateByUrl('pages/gestionbases/creacion');
  }

  removeBase(base) {
    const confirmModal = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Deshabilitar base',
        bodyText: '¿Está seguro de continuar?',
      },
    });
    confirmModal.afterClosed().subscribe((res) => {
      if (res === true) {
        this.basesService.deleteBase(base.baseId).subscribe(() => {
          this.getBases();
        });
      }
    });
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.bases);
  }

  // @SONAR_DECLARE_COLUM_TABLE_START@
  initializeColumns() {
    this.basesColumns = [
      {
        name: '#',
        dataKey: 'correlativo',
        position: 'left',
        isSortable: true,
        width: '5%',
      },
      {
        name: 'CÓDIGO',
        dataKey: 'codigoConvocatoria',
        position: 'left',
        isSortable: true,
        width: '8%',
      },
      {
        name: 'F. CREACIÓN',
        dataKey: 'fechaConvocatoria',
        position: 'left',
        isSortable: true,
        width: '10%',
      },
      {
        name: 'RÉGIMEN',
        dataKey: 'regimenLaboral',
        position: 'left',
        isSortable: true,
        width: '10%',
      },
      {
        name: 'VACANTES',
        dataKey: 'nroVacante',
        position: 'left',
        isSortable: true,
        width: '5%',
      },
      {
        name: 'GESTOR ORH',
        dataKey: 'gestor',
        position: 'left',
        isSortable: true,
        width: '15%',
      },
      {
        name: 'Coordinador',
        dataKey: 'coordinador',
        position: 'left',
        isSortable: true,
        width: '10%',
      },
      {
        name: 'F. PUBLICACIÓN',
        dataKey: 'fechaPublicacion',
        position: 'left',
        isSortable: true,
        width: '12%',
      },
    ];
  }
  // @SONAR_DECLARE_COLUM_TABLE_END@

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de bases';
    model.headers = [
      'ID',
      'CÓDIGO',
      'FECHA CREACIÓN',
      'RÉGIMEN',
      'VACANTES',
      'GESTOR ORH',
      'COORDINADOR',
      'F. PUBLICACIÓN',
      'ESTADO',
    ];
    model.keys = [
      'baseId',
      'codigoConvocatoria',
      'fechaConvocatoria',
      'regimenLaboral',
      'nroVacante',
      'gestor',
      'coordinador',
      'fechaPublicacion',
      'etapa',
    ];
    return model;
  }

  publicarBase(item: any) {
    const confirmModal = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Programar publicación ',
        bodyText: `Desea programar la publicación de la convocatoria`,
        rutaImagen: 'assets/images/icons/send.png',
      },
    });
    confirmModal.afterClosed().subscribe((res) => {
      if (res === true) {
        const persona: any = JSON.parse(sessionStorage.getItem('persona'));
        const request: any = {
            movimientoId: null,
            coordinadorId: persona.personaId,
            nombreCoordinador: persona.nombreCompleto,
            baseId: item.baseId,
            entidadId: this.authenticationRepository.getCurrentUserValue
              .entidadId,
            estadoOldId: item.etapaId,
            estadoNewId: this.estados.find(
              (est) => est.codProg === this.const.ETA_BASE_POR_PUBLICAR
            ).maeDetalleId,
        };
        this.basesService
          .publicarBase(request)
          .subscribe(
            () => {
              this.toastService.showToast(
                'Base Publicada correctamente',
                'success'
              );
              this.getBases();
            },
            (error) => this.toastService.showToast(error.message, 'danger')
          );
      }
    });
  }

  private mapObs(base, res: any) {
    const ultimoMovimiento = res[0];
    const penultimoMovimiento = res[1];
    if (
      penultimoMovimiento &&
      base.codigoEtapa === this.const.ETA_BASE_POR_REVISAR
    ) {
      penultimoMovimiento.observacionDTOList.map((obs) => {
        this.helperService.observaciones[obs.etapa - 1].description =
          obs.observacion;
        this.helperService.observaciones[obs.etapa - 1].resuelto = true;
      });
    } else {
      ultimoMovimiento.observacionDTOList.map((obs) => {
        this.helperService.observaciones[obs.etapa - 1].description =
          obs.observacion;
        this.helperService.observaciones[obs.etapa - 1].resuelto = false;
      });
    }
  }

  validRangeDateFormat(event: any) {
    this.rangePickerStatus = 'basic';
    const fecha = this.filterForm.controls['fecha'].value;

    if (this.filterForm.controls['fecha'].errors && fecha === null) {
      this.rangePickerStatus = 'danger';
    }
  }
}
