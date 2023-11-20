import { ConfiguracionReqMinRepository } from 'src/app/@domain/repository/configuracion-req-min.repository';
import { ModalElegirPlantillaComponent } from './modal-elegir-plantilla/modal-elegir-plantilla.component';
import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';

import { sortDataTableComponent } from 'src/app/utils/general';
import { Sort } from '@angular/material/sort';
import { TableColumn } from '../../@common-components/material-table/table-column';
import { ExportExcelModel } from '../../@service/export-excel.service';

import { CuentaEntidadRepository } from 'src/app/@domain/repository/cuenta.entidad.repository';
import { forkJoin } from 'rxjs';
import { EvaluacionesServirService } from 'src/app/@data/services/evaluaciones-servir.service';
import { Const } from 'src/app/@data/services/const';
import { ModalConfirmationComponent } from '../../@common-components/modal-confirmation/modal-confirmation.component';
import { EvaluacionConocimientosService } from 'src/app/@data/services/evaluacion-conocimientos.service';
import { ProcesoEvaluacionService } from 'src/app/@data/services/proceso-evaluacion.service';
import { PerfilesService } from 'src/app/@data/services/perfiles.service';
import { MaestraService } from 'src/app/@data/services/maestra.service';
import { MatDialog } from '@angular/material/dialog';
import { Router } from '@angular/router';
import { ToastService } from '../../@common-components/toast';

@Component({
  selector: 'serv-talento-evaluacion-configuracion',
  templateUrl: './evaluacion-configuracion.component.html',
  styleUrls: ['./evaluacion-configuracion.component.scss'],
})
export class EvaluacionConfiguracionComponent implements OnInit {
  filterForm: FormGroup;
  responsables = [];

  roles = [];
  regimenes = [];
  estadoReqMin = [];
  estadoEvaCur = [];

  const = Const;

  perfiles: any[] = [];
  perfilColumns: TableColumn[];

  constructor(
    private fb: FormBuilder,
    private cuentaAsociadaService: CuentaEntidadRepository,
    private evaluacionServirService: EvaluacionesServirService,
    private evaluacionConocimientosService: EvaluacionConocimientosService,
    private procesoEvaluacionService: ProcesoEvaluacionService,
    private perfilesService: PerfilesService,
    private maestraService: MaestraService,
    private router: Router,
    private matDialog: MatDialog,
    private configuracionReqMinService: ConfiguracionReqMinRepository,
    private dialog: MatDialog,
    private toastService: ToastService
  ) { }



  ngOnInit(): void {
    this.initializeForm();
    this.loadCombox();
    this.search();
  }

  get f() {
    return this.filterForm.controls;
  }

  loadCombox() {
    const getEstadoReqMin = this.procesoEvaluacionService.comboEstadoReqMinimos();
    const getEstadoEvaCurr = this.procesoEvaluacionService.comboEstadoEvaCurricular();
    const getListPerfilGrupo = this.perfilesService.getListaPerfilGrupo();
    const getRegimen = this.maestraService.getMaestraDetalleByCod(
      'TBL_REGIMEN'
    );
    forkJoin([
      getEstadoReqMin,
      getEstadoEvaCurr,
      getListPerfilGrupo,
      getRegimen,
    ]).subscribe((results) => {
      this.estadoReqMin = results[0];
      this.estadoEvaCur = results[1];
      this.roles = results[2];
      this.regimenes = results[3];
    });
  }

  getBuscarPerfil() {
    this.procesoEvaluacionService
      .listarConfiguracionPerfil(
        this.filterForm.controls.nombrePerfil.value,
        this.filterForm.controls.selRegimen.value,
        this.filterForm.controls.selRol.value,
        this.filterForm.controls.selEstadoRm.value,
        this.filterForm.controls.selEc.value
      )
      .subscribe((res) => {
        this.perfiles = [...res];
        this.perfiles.forEach((el) => {
          el.settings = {};
          if (!el.experienciaId) {
            el.settings = {};
            el.settings.disableEdit = true;
          } else {
            el.settings = {};
          }
        });
      });
  }

  setRoles(roles: any[]) {
    return roles.filter(
      (rol) =>
        rol.rolId === Const.R_COORDINADOR || rol.rolId === Const.R_GESTOR_ORH
    );
  }

  initializeForm() {
    this.filterForm = this.fb.group({
      selEstadoRm: '',
      selEc: '',
      nombrePerfil: '',
      selRegimen: '',
      selRol: '',
    });

    this.perfilColumns = [
      {
        name: 'NOMBRE DEL PERFIL',
        dataKey: 'nombrePerfil',
        position: 'left',
        isSortable: true,
        width: '25%',
      },
      {
        name: 'CÓDIGO CONVOCATORIA',
        dataKey: 'codigoConvocatoria',
        position: 'left',
        isSortable: true,
        width: '25%',
      },
      {
        name: 'RÉGIMEN',
        dataKey: 'regimen',
        position: 'left',
        isSortable: true,
        width: '20%',
      },
      {
        name: 'ROL',
        dataKey: 'rol',
        position: 'left',
        isSortable: true,
        width: '20%',
      },
      // {
      //   name: 'ESTADO RM',
      //   dataKey: 'descEstadoRm',
      //   position: 'left',
      //   isSortable: true,
      //   width: '10%',
      // },
      // {
      //   name: 'ESTADO EC',
      //   dataKey: 'descEstadoEc',
      //   position: 'left',
      //   isSortable: true,
      //   width: '10%',
      // },
    ];
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.perfiles);
  }

  configPerfil(e) {
    sessionStorage.setItem('selectedPerfil', JSON.stringify(e));
    this.configuracionReqMinService.obtenerPlantillaPorPerfil(e.perfilId, e.baseId).subscribe((mc) => {
      console.log(mc);
      if (mc.configPerfilId != null) {
        localStorage.setItem("selectedPlantilla", mc.tipoPerfil);
        localStorage.setItem("selectedconfigPerfilId", mc.configPerfilId);
        this.router.navigateByUrl("pages/configuracion-evaluacion/configuracion-requisitos-minimos");
      } else {
        this.matDialog.open(ModalElegirPlantillaComponent, { width: '500px' });
      }
    });

  }

  removePerfil(e) {
    const confirmModal = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Remover Perfil',
        bodyText:
          '¿Está seguro de querer remover el Perfil seleccionado?',
      },
    });
    confirmModal.afterClosed().subscribe((res) => {
      if (res) {
        this.configuracionReqMinService.setEliminarPerfil(e.perfilId, e.baseId).subscribe((mc) => {
          this.toastService.showToast(
            'La plantilla ha sido desactivada con éxito',
            'success'
          );
        });

      }
    });

  }

  clear() {
    this.initializeForm();
    this.search();
  }

  search() {
    this.getBuscarPerfil();
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de perfiles';
    model.headers = [
      'NOMBRE DEL PERFIL',
      'RÉGIMEN',
      'ROL',
      'ESTADO RM',
      'ESTADO EC',
    ];
    model.keys = [
      'nombrePerfil',
      'regimen',
      'rol',
      'descEstadoRm',
      'descEstadoEc',
    ];
    return model;
  }
}
