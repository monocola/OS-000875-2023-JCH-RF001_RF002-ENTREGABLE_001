import { Component, EventEmitter, Input, OnInit, Output, ViewChild } from '@angular/core';
import { MatTableDataSource } from '@angular/material/table';
import { Const } from '../../../@data/services/const';
import { MatSort, Sort } from '@angular/material/sort';
import { ExportExcelModel, ExportExcelService } from '../../@service/export-excel.service';
import { TableColumn } from '../../../@presentation/@common-components/material-table/table-column';
import { MatPaginator } from '@angular/material/paginator';
import { AuthenticationRepository } from '../../../@domain/repository/authentication.repository';
import { map, toArray } from 'rxjs/operators';
import { from } from 'rxjs';
import { FormBuilder, FormGroup } from '@angular/forms';
import { Router } from '@angular/router';
import { base64ToFilePromise, getBase64 } from 'src/app/utils/converterFile';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { OrganigramaRepository } from '../../../@domain/repository/organigrama.repository';
import * as FileSaver from 'file-saver';
import { ModalCreacionUoComponent } from './modal-creacion-uo/modal-creacion-uo.component';
import { MatDialog } from '@angular/material/dialog';
import { UnidadOrganicaRepository } from './../../../@domain/repository/unidad-organica.repository';
import { UnidadOrganicaCombo } from '../../../@data/model/unidadOrganicaCombo';
import { forkJoin } from 'rxjs';
import { MaestraParametro } from '../../../@data/model/maestra-parametro';
import { ServidoresCiviles } from '../../../@data/model/servidores-civiles';
import { ServidoresRepository } from '../../../@domain/repository/servidores.repository';
import { MaestraParametroRepository } from '../../../@domain/repository/maestra-parametro.repository';
import { ModalRemoveComponent } from './modal-remove/modal-remove.component';
import { Organigrama } from '../../../@data/model/organigrama';
import { Utils } from 'src/app/utils/utils';

@Component({
  selector: 'serv-talento-organigrama',
  templateUrl: './organigrama.component.html',
  styleUrls: ['./organigrama.component.scss']
})
export class OrganigramaComponent implements OnInit {

  organos: MaestraParametro[];
  dataToEdit: any = null;
  length = 100;
  pageSize = 10;
  pageSizeOptions: number[] = [5, 10, 25];
  unidadesOrganicas: any[];
  estados: any[];
  customColumn = 'id';
  defaultColumns = ['descripcion', 'nivel', 'sigla', 'naturaleza'];
  defaultColumnsNames = ['DESCRIPCIÓN', 'NIVEL', 'SIGLA', 'NATURALEZA'];
  customColumn2 = 'ACCIONES';
  customColumn3 = 'ESTADO';
  acciones = true;
  paginationSizes: number[] = [5, 10, 15];
  defaultPageSize = this.paginationSizes[1];
  tableDataSource = new MatTableDataSource([]);
  displayedColumns: string[];
  fontSize = 'fs-13';
  filterForm: FormGroup = null;
  organosPadre: any[] = [];
  data2: any[] = [];
  errorsFromFile: any[] = [];
  fileOrgano: File = null;
  fileOrganoBase64 = '';
  ordersTableColumns: TableColumn[];
  organigrama: any[] = [];
  tipoOrganoId: number = 0;
  unidadOrganicaCbo: UnidadOrganicaCombo[];
  unidadOrganicaCboTemp: UnidadOrganicaCombo[];
  unidadOrganicaSup: UnidadOrganicaCombo[];
  lstServCiviles: ServidoresCiviles[] = [];
  searchMode = false;
  const = Const;
  ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
  cicloDefaultDesc: string;
  cicloDefault: number;

  constructor(
    private exportExcelService: ExportExcelService,
    private authRepository: AuthenticationRepository,
    private fb: FormBuilder,
    private router: Router,
    private toastService: ToastService,
    private organigramaRepository: OrganigramaRepository,
    private dialog: MatDialog,
    private UnidadOrganicaRepository: UnidadOrganicaRepository,
    private servidoresRepository: ServidoresRepository,
    private maeParametroRepository: MaestraParametroRepository,
  ) {
    this.initializeForm();
   }

  ngOnInit(): void {
    this.loadCombox();
   // this.initForm();
    this.initializeColumns();
    this.search();
  }

  initializeForm() {
    this.filterForm = this.fb.group({
      tipoOrgano: '',
      nombreUO: '',
      siglaUO: '',
      siglaUOSup: '',
      tipoOrganoId: '',
      unidadOrganicaSuperiorId: '',
      unidadOrganicaId: ''
    });
    
  }

  setCiclo() {
    if (this.ciclo.length !== 0 && this.ciclo) {
      this.cicloDefaultDesc = ' - ' + this.ciclo.anio;
      this.cicloDefault = this.ciclo.cicloId;
    } else {
      this.cicloDefaultDesc = '';
    }
  }


  initForm() {
    this.filterForm = this.fb.group({
      personaId: '',
      organigramaId: '',
      unidadId: '',
      puesto: '',
      estado: '',
      tipoOrganoId: '',
      unidadOrganicaSuperiorId: '',
      unidadOrganicaId: ''
    });
 
  }

  initializeColumns() {
    this.ordersTableColumns = [
      { name: 'Tipo de Órgano', dataKey: 'tipoOrgano', position: 'left', isSortable: true },
      { name: 'Nombre de Órgano / UO / Sub UO', dataKey: 'nombreUO', position: 'left', isSortable: true },
      { name: 'Siglas del órgano / UO / Sub UO', dataKey: 'siglaUO', position: 'left', isSortable: true },
      { name: 'Siglas del Organo / UO Superior', dataKey: 'siglaUOSup', position: 'left', isSortable: true },
    ];
  }

  loadCombox() {
    this.searchMode = false;
    const getOrganos = this.maeParametroRepository.getMaestraParametro('TIPO_NATURALEZA');
    const getUndOrganicaCbo = this.UnidadOrganicaRepository.getUnidadOrganicaCbo();
    const getUndOrganicaSup = this.UnidadOrganicaRepository.getUnidadOrganicaSup();

    forkJoin([getOrganos, getUndOrganicaCbo, getUndOrganicaSup, ]).subscribe(
      (results ) => {
        this.organos = results[0];
        this.unidadOrganicaCbo = results[1];
        this.unidadOrganicaSup = results[2];
        this.unidadOrganicaCboTemp = results[2];
        this.setCiclo();

      },
      (error) => {}
    );
  }

  get f() {
    return this.filterForm.controls;
  }

  files: File[] = [];

  downloadFile() {
    this.organigramaRepository.downloadExcel().subscribe(
      (res) => {
        const nameFile = `Plantilla del Organigrama.xlsx`;
        const rutaFile = `data:application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;base64,${res}`;
        base64ToFilePromise(rutaFile, nameFile, '').then((file) =>
          FileSaver.saveAs(file)
        );
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  openModalRegister(createMode: boolean = true) {
    const registerDialog = this.dialog.open(ModalCreacionUoComponent, {
      data: {
        createMode,
        estados: this.estados,
        dataToEdit: this.dataToEdit
      },
    });

    registerDialog.afterClosed().subscribe((res) => {
      this.dataToEdit = null;
      if (res) {
        this.initializeForm();
        this.search();
        this.loadCombox();
      }
    });
  }

  clear() {
    if (this.organigrama.length !== 0) {
      this.organigrama = [];
    }
    this.initForm();
    this.search();
    this.tipoOrganoChange(null);
  }

  search() {
    this.searchMode = true;
    const body = this.filterForm.getRawValue();
    this.organigramaRepository.getGestionOrganigramaByFiltro(body).subscribe(
      (res) => {
        this.organigrama = res;
        if (this.organigrama.length === 0) {
          this.toastService.showToast(
            'No se encontraron resultados',
            'primary'
          );
        }
      },
      (err) => {
        this.toastService.showToast(err.message, 'danger');
      }
    );
  }
 
  tipoOrganoChange(tipoOrganoId: number) {
    this.tipoOrganoId = tipoOrganoId;
    const getUndOrganicaSup = this.UnidadOrganicaRepository.getUnidadOrganicaSup(tipoOrganoId);
    const getUndOrganicaCbo = this.UnidadOrganicaRepository.getUnidadOrganicaCbo(tipoOrganoId);
  
    forkJoin([getUndOrganicaSup, getUndOrganicaCbo]).subscribe(
      (results ) => {
        this.unidadOrganicaSup = null;
        this.unidadOrganicaCbo = results[1];
        this.llenarSubUO();

      },
      (error) => {}
    );
  }

  llenarSubUO () {
    var idSuperiores = [];
    console.log(" this.unidadOrganicaCbo:", this.unidadOrganicaCbo)
    this.unidadOrganicaCbo.forEach(function (element, index, array) {
      idSuperiores.push(element.uoSupId); 
   });

   console.log("idSuperiores",idSuperiores)
   const idSuperioresArr = new Set(idSuperiores);

   let ArrayId = [...idSuperioresArr];
   var unidadSup: UnidadOrganicaCombo[]=[];
   const result = this.unidadOrganicaCboTemp.filter(unidad => {
    for (var i = 0; i< ArrayId.length ; i++) {
     if(unidad.id == ArrayId[i]) {
      unidadSup.push(unidad);
     }
    }
   }
    );
    this.unidadOrganicaSup = [...unidadSup];

    unidadSup = null;
    idSuperiores = null;
  }
 
  sortData($event) {}

  unidadOrganicaSuperiorChange(unidadOrganicaSuperiorId: number) {
    this.UnidadOrganicaRepository.getUnidadOrganicaCbo(this.tipoOrganoId, unidadOrganicaSuperiorId)
      .subscribe(result => {
        this.unidadOrganicaCbo = result;
      });
  }

  editUO(item: any) {
    this.dataToEdit = item;
    this.openModalRegister(false);
  }

  removeGO(item: any) {
    const confirmModal = this.dialog.open(ModalRemoveComponent, {
      data: {
        title: 'Eliminar Gestión de Organigrama',
        bodyText: '¿Está seguro de continuar?',
      },
    });
    confirmModal.afterClosed().subscribe((res) => {
      if (res === true) {
        this.organigramaRepository.deleteGO(item.organigramaId).subscribe((res) => {
            if ( res === true) {
              this.toastService.showToast(
                'Se realizó la eliminación exitosamente',
                'success'
              );
              this.initializeForm();
              this.search();
            } else {
              this.toastService.showToast(
                'La gestión de organigrama tiene dependencias',
                'danger'
              );
            }
        }, (err) => {
            this.toastService.showToast(err.message, 'danger');
        }
        );
      }
    });

  }

}
