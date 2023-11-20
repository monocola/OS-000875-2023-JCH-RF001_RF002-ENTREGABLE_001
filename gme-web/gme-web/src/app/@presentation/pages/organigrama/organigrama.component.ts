import { Component, OnInit, ViewChild } from '@angular/core';
// import { MatTableDataSource } from '@angular/material/table';
import { Const } from '../../../@data/services/const';
import { ExportExcelService } from '../../@service/export-excel.service';
import { TableColumn } from '../../@common-components/material-table/table-column';

import { AuthenticationRepository } from '../../../@domain/repository/authentication.repository';

import { FormBuilder, FormGroup } from '@angular/forms';
import { Router } from '@angular/router';
import { base64ToFilePromise } from 'src/app/utils/converterFile';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { OrganigramaRepository } from '../../../@domain/repository/organigrama.repository';
import * as FileSaver from 'file-saver';
import { ModalCreacionUoComponent } from './modal-creacion-uo/modal-creacion-uo.component';
import { MatDialog } from '@angular/material/dialog';
import { UnidadOrganicaRepository } from '../../../@domain/repository/unidad-organica.repository';
import { UnidadOrganicaCombo } from '../../../@data/model/unidadOrganicaCombo';
import { forkJoin } from 'rxjs';
import { MaestraParametro } from '../../../@data/model/maestra-parametro';
import { ServidoresCiviles } from '../../../@data/model/servidores-civiles';
import { ServidoresRepository } from '../../../@domain/repository/servidores.repository';
import { MaestraParametroRepository } from '../../../@domain/repository/maestra-parametro.repository';
import { ModalRemoveComponent } from './modal-remove/modal-remove.component';
import { Organigrama } from '../../../@data/model/organigrama';
import { ModalConfirmacionComponent } from './modal-confirmacion/modal-confirmacion.component';
import { ModalConfirmacionTodoBienComponent } from './modal-confirmacion-todo-bien/modal-confirmacion-todo-bien.component';
import { FileDropzoneComponent } from '../../@common-components/file-dropzone/file-dropzone.component';
import { MatTableDataSource } from '@angular/material/table';

@Component({
  selector: 'gme-web-organigrama',
  templateUrl: './organigrama.component.html',
  styleUrls: ['./organigrama.component.scss']
})
export class OrganigramaComponent implements OnInit {
  @ViewChild(FileDropzoneComponent) fileDropzone: FileDropzoneComponent;
  holderText = 'Buscar por Tipo de órgano, nombre de O / UO / Sub UO, Siglas de O / UO / Sub...';

  color = "accent";
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
  organigrama: Organigrama[] = [];
  organigrama1: Organigrama[] = [];
  tipoOrganoId: number = 0;
  unidadOrganicaCbo: UnidadOrganicaCombo[];
  unidadOrganicaSup: UnidadOrganicaCombo[];
  unidadOrganicaCboTemp: UnidadOrganicaCombo[];

  lstServCiviles: ServidoresCiviles[] = [];
  searchMode = false;
  const = Const;
  constructor(
    private exportExcelService: ExportExcelService,
    private authRepository: AuthenticationRepository,
    private fb: FormBuilder,
    private router: Router,
    private toastService: ToastService,
    private organigramaRepository: OrganigramaRepository,
    private dialog: MatDialog,
    private unidadOrganicaRepository: UnidadOrganicaRepository,
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
      unidadOrganicaId: '',
      idUO: ''
    });
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
      { name: 'Tipo de órgano', dataKey: 'tipoOrgano', position: 'left', isSortable: true },
      { name: 'Nombre de órgano / UO / Sub UO', dataKey: 'nombreUO', position: 'left', isSortable: true },
      { name: 'Siglas del órgano / UO / Sub UO', dataKey: 'siglaUO', position: 'left', isSortable: true },
      { name: 'Siglas del órgano / UO superior', dataKey: 'siglaUOSup', position: 'left', isSortable: true },
    ];
  }

  loadCombox() {
    this.searchMode = false;
    const getOrganos = this.maeParametroRepository.getMaestraParametro('TIPO_NATURALEZA');

    const getUndOrganicaCbo = this.unidadOrganicaRepository.getUnidadOrganicaCbo();
    const getUndOrganicaSup = this.unidadOrganicaRepository.getUnidadOrganicaSup();

    forkJoin([getOrganos, getUndOrganicaCbo, getUndOrganicaSup,]).subscribe(
      (results) => {
        this.organos = results[0];
        this.unidadOrganicaCbo = results[1];
        this.unidadOrganicaSup = results[2];
        this.unidadOrganicaCboTemp = results[2];

      },
      (error) => { console.log(error); }
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
        // Se registró una sede
        this.initializeForm();
        this.search();
        this.loadCombox();
        this.clear();

        this.holderText === null;
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
        this.organigrama.forEach(x => x.siglaUOSup = x.siglaUOSup ?? 'No aplica');
        //  this.organigrama.filter(x => x.siglaUOSup = x.siglaUOSup ?? 'No aplica');
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

    if (tipoOrganoId === null) {

      this.tipoOrganoId = tipoOrganoId;
      const getUndOrganicaSup = this.unidadOrganicaRepository.getUnidadOrganicaSup(tipoOrganoId);
      const getUndOrganicaCbo = this.unidadOrganicaRepository.getUnidadOrganicaCbo(tipoOrganoId);

      console.log("entre", tipoOrganoId)

      forkJoin([getUndOrganicaSup, getUndOrganicaCbo]).subscribe(
        (results) => {
          this.unidadOrganicaSup = results[0];
          this.unidadOrganicaCbo = results[1];
          this.f['unidadOrganicaSuperiorId'].setValue('');
          this.f['unidadOrganicaId'].setValue('');
        },
        (err) => this.toastService.showToast(err, 'danger')
      );

    } else {

      this.tipoOrganoId = tipoOrganoId;
      const getUndOrganicaSup = this.unidadOrganicaRepository.getUnidadOrganicaSup(tipoOrganoId);
      const getUndOrganicaCbo = this.unidadOrganicaRepository.getUnidadOrganicaCbo(tipoOrganoId);

      console.log("entre", tipoOrganoId)

      forkJoin([getUndOrganicaSup, getUndOrganicaCbo]).subscribe(
        (results) => {
          // this.unidadOrganicaSup = results[0];

          this.unidadOrganicaSup = null;
          this.unidadOrganicaCbo = results[1];
          this.llenarSubUO();
          this.f['unidadOrganicaSuperiorId'].setValue('');
          this.f['unidadOrganicaId'].setValue('');
        },
        (err) => this.toastService.showToast(err, 'danger')
      );

    }
  }


  llenarSubUO() {
    var idSuperiores = [];
    console.log(" this.unidadOrganicaCbo:", this.unidadOrganicaCbo)
    this.unidadOrganicaCbo.forEach(function (element, index, array) {
      idSuperiores.push(element.uoSupId);
    });

    console.log("idSuperiores", idSuperiores)
    const idSuperioresArr = new Set(idSuperiores);

    let ArrayId = [...idSuperioresArr];
    var unidadSup: UnidadOrganicaCombo[] = [];
    const result = this.unidadOrganicaCboTemp.filter(unidad => {
      for (var i = 0; i < ArrayId.length; i++) {
        if (unidad.id == ArrayId[i]) {
          unidadSup.push(unidad);
        }
      }
    }
    );
    this.unidadOrganicaSup = [...unidadSup];

    unidadSup = null;
    idSuperiores = null;
  }

  sortData($event) {
  }

  unidadOrganicaSuperiorChange(unidadOrganicaSuperiorId: number) {
    this.unidadOrganicaRepository.getUnidadOrganicaCbo(this.tipoOrganoId, unidadOrganicaSuperiorId)
      .subscribe(result => {
        console.log('%corganigrama.component.ts line:250 result', 'color: red;', result);
        this.unidadOrganicaCbo = result;
        this.f['unidadOrganicaId'].setValue('');
      });
  }

  editUO(item: any) {
    this.dataToEdit = item;
    console.log("data edita", this.dataToEdit)
    this.openModalRegister(false);
    if (this.dataToEdit === item) {

      this.clear();
    }

  }

  removeGO(item: any) {
    const confirmModal = this.dialog.open(ModalRemoveComponent, {
      data: {
        bodyText: 'Se eliminará el siguiente registro ¿Estás seguro de realizar la siguiente acción?',
      },
    });
    confirmModal.afterClosed().subscribe((res) => {
      if (res === true) {
        this.organigramaRepository.deleteGO(item.organigramaId)
          .subscribe((resp) => {
            if (resp.status.success) {
              this.toastService.showToast(
                'Se realizó la eliminación exitosamente',
                'success'
              );
              this.initializeForm();
              this.search();
              this.clear();
            } else {
              if (resp.status.error.messages[0]) {
                this.toastService.showToast(resp.status.error.messages[0], 'danger');
              } else {
                this.toastService.showToast('La gestión de organigrama tiene dependencias', 'danger');
              }
            }
          },
            (err) => {
              this.toastService.showToast(err.message, 'danger');
            }
          );
      }
    });
  }

  upload(file: any) {

    this.organigramaRepository
      .uploadFileMasivo(file)
      .subscribe(
        (res) => {
          console.log(res);
          if (res === true) {
            this.openDialogOk();
            this.clear();


          } else {
            this.openDialog(res.archivo);
            this.clear();

          }
        },
        (err) => {
          this.toastService.showToast(
            'Ocurrió un error en el servidor',
            'danger', err
          );
        }
      );
  }
  openDialog(archivo: any[]) {
    this.dialog.open(ModalConfirmacionComponent, {
      disableClose: true,
      data: archivo
    }).afterClosed().subscribe(item => {
      this.fileDropzone.removeFile();
      this.search();
    });

  }

  openDialogOk() {
    this.dialog.open(ModalConfirmacionTodoBienComponent, {
      disableClose: true,
      data: this.fileOrganoBase64
    }).afterClosed().subscribe(item => {
      this.fileDropzone.removeFile();
      this.search();
    });
  }



}
