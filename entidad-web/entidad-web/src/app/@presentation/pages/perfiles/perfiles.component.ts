import { Component, OnInit, ViewEncapsulation } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { Sort } from '@angular/material/sort';
import { Router } from '@angular/router';
import { Const } from 'src/app/@data/services/const';
import { PerfilesRepository } from 'src/app/@domain/repository/perfiles.repository';
import { TableColumn } from 'src/app/@presentation/@common-components/material-table/table-column';
import { ExportExcelModel } from 'src/app/@presentation/@service/export-excel.service';
import { sortDataTableComponent } from 'src/app/utils/general';
import { ModalConfirmationComponent } from '../../@common-components/modal-confirmation/modal-confirmation.component';
import { DuplicarModalComponent } from './duplicar-modal/duplicar-modal.component';
import { HelperPerfilesService } from './helperPerfiles.service';
import { HelperLey1401Service } from './ley1401/helperLey1401.service';
import { HelperLey276Service } from './ley276/helperLey276.service';
import { HelperLey30057Service } from './ley30057/helperLey30057.service';
import { FileVisualizerComponent } from '../../@common-components/file-visualizer/file-visualizer.component';
import { BasesRepository } from 'src/app/@domain/repository/bases.repository';
import { RegistroMasivoModalComponent } from './registro-masivo-modal/registro-masivo-modal.component';
 
@Component({
  selector: 'serv-talento-perfil',
  templateUrl: './perfiles.component.html',
  styleUrls: ['./perfiles.component.scss'],
  encapsulation: ViewEncapsulation.None,
})
export class PerfilesComponent implements OnInit {
  filterForm: FormGroup;
  perfiles: any[] = [];
  perfilesTableColumns: TableColumn[];

  searchMode = false;
  rangePickerStatus: string = 'basic';
  showAlert: boolean = false;
  visibleFilters: boolean = false;
  isPerfiles: boolean = true;
  reg_correctos: number = 0;

  estadosRevisado: any [] = [{
    id: Const.EST_PERFILES_REVISADO,
    texto: 'Revisado'
  }, {
    id: Const.EST_PERFILES_POR_REVISAR,
    texto: 'No Revisado'
  }];

  origenes: any [] = [{
    id: Const.ORI_PERFILES_INDIVIDUAL,
    texto: 'Individual'
  }, {
    id: Const.ORI_PERFILES_MASIVO,
    texto: 'Masivo'
  }];
  constructor(
    private fb: FormBuilder,
    public helperPerfilesService: HelperPerfilesService,
    public perfilesService: PerfilesRepository,
    private ley30057Service: HelperLey30057Service,
    private ley1401Service: HelperLey1401Service,
    private otrasLeyesService: HelperLey276Service,
    private basesService: BasesRepository,
    private dialog: MatDialog,
    private router: Router
  ) {}

  ngOnInit(): void {
    this.initializeForm();
    this.initializeColumns();
    this.helperPerfilesService.loadCombox();
    this.listarPerfiles();
  }

  get f() {
    return this.filterForm.controls;
  }

  initializeForm() {
    this.filterForm = this.fb.group({
      regimen: '',
      organo: '',
      unidadOrganica: '',
      codigo: '',
      nombrePerfil: '',
      fecha: '',
      estado: '',
      origenPerfil: '',
      estadoRevision: ''
    });
  }

  listarPerfiles() {
    this.perfilesService.getPerfiles().subscribe(
      (res) => {
        this.perfiles = res;
        this.formatColor();
      },
      (err) => {}
    );
  }

  search() {
    this.perfilesService.getPerfiles(this.filterForm.value).subscribe(
      (res: any[]) => {
        this.perfiles = res;
        this.searchMode = true;
        this.formatColor();
      },
      (err) => {}
    );
  }

  formatColor() {
    this.perfiles.forEach((el) => {
      el.origenPerfilDescripcion =
        el.origenPerfil === Const.ORI_PERFILES_INDIVIDUAL
          ? 'Individual'
          : 'Masivo';
      if (el.estadoRevision === null || el.estadoRevision === Const.EST_PERFILES_POR_REVISAR) {
        el.estadoRevisionDescripcion = 'Por Revisar';
      } else {
        el.estadoRevisionDescripcion = 'Revisado';
      }
      
      if (el.estadoId === '0') {
        el.settings = {};
        el.settings.disableDelete = true;
        el.settings.estado = {};
        el.settings.estado.color = '#eb5757';
      } else {
        el.settings = {};
        el.settings.estado = {};
        el.settings.estado.color = '#0d88bc';
      }
    });
  }

  clear() {
    this.searchMode = false;
    this.listarPerfiles();
    this.initializeForm();
  }

  changeFormValue(type: number) {
    type === 1
      ? this.filterForm.get('organo').patchValue('')
      : this.filterForm.get('unidadOrganica').patchValue('');
  }

  // ---------------------------------- TABLA ---------------------------------- //

  editPerfil(perfil: any) {
    const value = perfil.codProg;
    const regimen = this.helperPerfilesService.regimenes.find(
      (e) => e.codProg === value
    );
    sessionStorage.setItem('regimenSelected', JSON.stringify(regimen));
    switch (value) {
      case Const.MD_DL30057:
        this.ley30057Service.idIdentificacion = perfil.perfilId;
        this.router.navigateByUrl('pages/regperfilpuesto/creacion/ley30057');
        break;
      case Const.MD_DL1041:
        this.ley1401Service.idIdentificacion = perfil.perfilId;
        this.router.navigateByUrl('pages/regperfilpuesto/creacion/ley1401');
        break;
      default:
        this.otrasLeyesService.idIdentificacion = perfil.perfilId;
        this.router.navigateByUrl('pages/regperfilpuesto/creacion/otras');
        break;
    }
  }

  removePerfil(perfil: any) {
    const confirmModal = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Deshabilitar perfil',
        bodyText: '¿Está seguro de continuar?',
      },
    });
    confirmModal.afterClosed().subscribe((res) => {
      if (res === true) {
        this.perfilesService.inactivatePerfil(perfil.perfilId).subscribe(() => {
          this.listarPerfiles();
        });
      }
    });
  }

  copiarPerfil(perfil: any) {
    const duplicarModal = this.dialog.open(DuplicarModalComponent, {
      width: '600px',
      data: {
        perfil,
        regimenes: this.helperPerfilesService.regimenes,
      },
    });
    duplicarModal.afterClosed().subscribe((res) => {
      if (res) {
        this.duplicatePerfil(res);
      }
    });
  }

  showPdf(base) {
    let doc =
      base.puestoCodigo === null ? 'N° XXXX-XXXX-MUN' : base.puestoCodigo;
    let documento =
      'Perfil ' + doc.substring(doc.indexOf('N°'), doc.lastIndexOf('-'));

    this.basesService.obtenerPdfNew(base.perfilId).subscribe((dataPDF) => {
      this.dialog.open(FileVisualizerComponent, {
        data: {
          base64String: dataPDF,
          filename: documento,
          extension: 'pdf',
        },
      });
    });
  }

  duplicatePerfil(regimen) {
    sessionStorage.setItem('regimenSelected', JSON.stringify(regimen));
    if (regimen.codProg === Const.MD_DL30057) {
      this.ley30057Service.idIdentificacion = regimen.perfilId;
      this.ley30057Service.duplicarMode = true;
      this.router.navigateByUrl('pages/regperfilpuesto/creacion/ley30057');
    } else {
      if (regimen.codProg === Const.MD_DL1041) {
        this.ley1401Service.idIdentificacion = regimen.perfilId;
        this.ley1401Service.duplicarMode = true;
        this.router.navigateByUrl('pages/regperfilpuesto/creacion/ley1401');
      } else {
        this.otrasLeyesService.idIdentificacion = regimen.perfilId;
        this.otrasLeyesService.duplicarMode = true;
        this.router.navigateByUrl('pages/regperfilpuesto/creacion/otras');
      }
    }
  }

  initializeColumns() {
    // @SONAR_DECLARE_COLUM_TABLE_START@
    this.perfilesTableColumns = [
      {
        name: 'CÓDIGO',
        dataKey: 'puestoCodigo',
        position: 'left',
        isSortable: true,
        width: '5%',
      },
      {
        name: 'PERFIL',
        dataKey: 'nombrePuesto',
        position: 'left',
        isSortable: true,
        width: '15%',
      },
      {
        name: 'ORIGEN',
        dataKey: 'origenPerfilDescripcion',
        position: 'left',
        isSortable: true,
        width: '5%',
      },
      {
        name: 'REGIMEN',
        dataKey: 'regimenLaboral',
        position: 'left',
        isSortable: true,
        width: '10%',
      },
      {
        name: 'ÓRGANO',
        dataKey: 'siglaOrgano',
        position: 'left',
        isSortable: true,
        width: '10%',
      },
      {
        name: 'UNIDAD ORGÁNICA',
        dataKey: 'unidadOrganica',
        position: 'left',
        isSortable: true,
        width: '20%',
      },
      {
        name: 'POS',
        dataKey: 'nroPosicionesPuesto',
        position: 'left',
        isSortable: true,
        width: '5%',
      },
      {
        name: 'ESTADO',
        dataKey: 'estado',
        position: 'left',
        isSortable: true,
        width: '5%',
      },
    ];
  }
  // @SONAR_DECLARE_COLUM_TABLE_END@

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.perfiles);
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de perfiles';
    model.headers = [
      'CÓDIGO',
      'PERFIL',
      'ORIGEN',
      'RÉGIMEN',
      'SIGLA ÓRGANO',
      'UNIDAD ORGÁNICA',
      'NÚMERO DE PUESTOS',
      'ESTADO REVISIÓN',
      'ESTADO',
    ];
    model.keys = [
      'puestoCodigo',
      'nombrePuesto',
      'origenPerfilDescripcion',
      'regimenLaboral',
      'siglaOrgano',
      'unidadOrganica',
      'nroPosicionesPuesto',
      'estadoRevisionDescripcion',
      'estado',
    ];
    return model;
  }

  canDeactivate() {
    // this.helperPerfilesService.initializeValues();
    return true;
  }

  validRangeDateFormat(event: any) {
    this.rangePickerStatus = 'basic';
    const fecha = this.filterForm.controls['fecha'].value;

    if (this.filterForm.controls['fecha'].errors && fecha === null) {
      this.rangePickerStatus = 'danger';
    }
  }

  openModalRegistroMasivo() {
    const dialogReg = this.dialog.open(RegistroMasivoModalComponent, {
      data: {},
    });

    dialogReg.afterClosed().subscribe((result: any) => {
      if (result && result > 0) {
        this.listarPerfiles();
        this.reg_correctos = result;
        this.showAlert = true;
        setTimeout(() => {
          this.showAlert = false;
        }, 2000);
      }
    });
  }

  verificarPerfil(event: any) {
    if (event.revisionHabilitado === true) {
      const confirmModal = this.dialog.open(ModalConfirmationComponent, {
        data: {
          rutaImagen: 'assets/images/icons/check.png',
          title: 'Verificación de perfil',
          bodyHtmlText: `Confirma que los <strong>datos</strong> ingresados en el perfil "<span class="color-primary">${event.nombrePuesto}</span>" son <strong>correctos</strong>`,
        },
      });
  
      confirmModal.afterClosed().subscribe((res) => {
        if (res === true) {
          this.perfilesService
            .revisarPerfil(event.perfilId, Const.EST_PERFILES_REVISADO)
            .toPromise()
            .then((res: any) => {
              this.listarPerfiles();
            })
            .catch((error: any) => {});
        }
      });
    } else {
      const confirmModal = this.dialog.open(ModalConfirmationComponent, {
        data: {
          rutaImagen: 'assets/images/icons/alert.png',
          title: 'Perfil incompleto',
          textOk: 'Completar perfil',
          bodyHtmlText: `
            Es necesario completar los siguientes datos <strong>registro</strong> del perfil para continuar con la <strong>revisión:</strong>
            ${this.getLiFaltantes (event)}
            `,
        },
      });
  
      confirmModal.afterClosed().subscribe((res) => {
        if (res === true) {
          this.editPerfil (event);
        }
      });
    }
  }

  getLiFaltantes (event: any) {
    let returned: string = '';

    returned += '<ul>';
    
    if (event.estadoExp && event.estadoExp === Const.INACTIVO) {
      if (event.codProg === Const.MD_DL1041) {
        returned += "<li class='text-left'><strong>Habilidades</strong></li>";
      } else {
        returned += "<li class='text-left'><strong>Experiencia</strong></li>";
      }
    }

    if (event.estadoForm && event.estadoForm === Const.INACTIVO) {
      returned += "<li class='text-left'><strong>Formación academica</strong></li>";
    }

    if (event.estadoFunc && event.estadoFunc === Const.INACTIVO) {
      returned += "<li class='text-left'><strong>Funciones / Actividades</strong></li>";
    }

    returned += '</ul>';

    return returned;
  }
}

export const perfiles = [];
