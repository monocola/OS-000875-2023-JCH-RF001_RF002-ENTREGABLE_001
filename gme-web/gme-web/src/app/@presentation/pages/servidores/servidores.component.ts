import { Component, OnInit, ViewChild, EventEmitter, Output } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { MaestraParametro } from '../../../@data/model/maestra-parametro';
import { MaestraParametroRepository } from '../../../@domain/repository/maestra-parametro.repository';
import { forkJoin } from 'rxjs';
import { TableColumn } from 'src/app/@presentation/@common-components/material-table/table-column';
import { ServidorEntidad } from '../../../@data/model/servidoresentidad';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { base64ToFilePromise } from 'src/app/utils/converterFile';
import { ServidoresRepository } from '../../../@domain/repository/servidores.repository';
import * as FileSaver from 'file-saver';

import { MatDialog } from '@angular/material/dialog';

import { ServidoresCiviles } from '../../../@data/model/servidores-civiles';
import { Sort } from '@angular/material/sort';
import { sortDataTableComponent } from '../../../utils/general';
import { UnidadOrganicaRepository } from '../../../@domain/repository/unidad-organica.repository';
import { UnidadOrganicaCombo } from '../../../@data/model/unidadOrganicaCombo';
import { ModalServidorComponent } from './modal-servidor/modal-servidor.component';
import { ModalEliminarComponent } from './modal-eliminar/modal-eliminar.component';
import { ActivatedRoute, Router } from '@angular/router';
import { ModalConfirmacionTodoBienComponent } from '../../@common-components/modal-confirmacion-todo-bien/modal-confirmacion-todo-bien.component';
import { ModalConfirmacionComponent } from '../../@common-components/modal-confirmacion/modal-confirmacion.component';
import { FileDropzoneComponent } from '../../@common-components/file-dropzone/file-dropzone.component';

@Component({
  selector: 'gme-web-servidores',
  templateUrl: './servidores.component.html',
  styleUrls: ['./servidores.component.scss']
})
export class ServidoresComponent implements OnInit {
  @ViewChild(FileDropzoneComponent) fileDropzone: FileDropzoneComponent;
  holderText = 'Buscar por N° Doc., Apellidos y nombres, UO, Puesto, Estado..';

  public model = null;

  @Output() closeOrgano = new EventEmitter();
  @Output() updateOrganos = new EventEmitter();
  @ViewChild('autoInput') input;

  organos: MaestraParametro[];
  organica: MaestraParametro[];
  organicaSup: MaestraParametro[];
  regimenLaboral: MaestraParametro[];
  tipoDocumento: MaestraParametro[];
  estados: MaestraParametro[];
  unidadOrganicaCbo: UnidadOrganicaCombo[];
  unidadOrganicaSup: UnidadOrganicaCombo[];
  unidadOrganicaCboTemp: UnidadOrganicaCombo[];

  servidores: ServidorEntidad[] = [];
  fileOrgano: File = null;
  fileOrganoBase64 = '';


  /**************************/
  lstServCiviles: ServidoresCiviles[] = [];
  lstServCiviles1: ServidoresCiviles[] = [];

  searchMode = false;
  ordersTableColumns: TableColumn[];
  filterForm: FormGroup = null;
  dataToEdit: ServidoresCiviles = null;
  dataToDelete: ServidoresCiviles = null;
  /***************************/
  tipoOrganoId: number = 0;

  constructor(
    private fb: FormBuilder,
    private maeParametroRepository: MaestraParametroRepository,
    private servidoresRepository: ServidoresRepository,
    private toastService: ToastService,
    private unidadOrganicaRepository: UnidadOrganicaRepository,
    public dialog: MatDialog,
    private router: Router,
    private activatedRoute: ActivatedRoute
  ) { }

  ngOnInit(): void {
    this.loadCombox();
    this.initForm();
    this.initializeColumns();
    this.search();
  }

  initializeColumns() {
    this.ordersTableColumns = [
      { name: 'N° Doc.', dataKey: 'docEntidadId', position: 'left', isSortable: true },
      { name: 'Apellidos y nombres', dataKey: 'apellidosNombres', position: 'left', isSortable: true },
      { name: 'Órgano/UO/Sub UO', dataKey: 'unidadOrganica', position: 'left', isSortable: true },
      { name: 'Puesto', dataKey: 'puesto', position: 'left', isSortable: true },
      { name: 'Estado', dataKey: 'estado', position: 'left', isSortable: true },
    ];
  }

  loadCombox() {
    this.searchMode = false;
    const getOrganos = this.maeParametroRepository.getMaestraParametro('TIPO_NATURALEZA');
    const getTipoDocumento = this.servidoresRepository.getTiposDocumento();
    const getRegimenLaboral = this.maeParametroRepository.getMaestraParametro('TIPO_REGIMEN_LABORAL');
    const getUndOrganicaCbo = this.unidadOrganicaRepository.getUnidadOrganicaCbo();
    const getUndOrganicaSup = this.unidadOrganicaRepository.getUnidadOrganicaSup();
    const getEstados = this.maeParametroRepository.getMaestraParametro('ESTADO_SERVIDOR_CIVIL');
    forkJoin([getOrganos, getTipoDocumento, getRegimenLaboral,
      getUndOrganicaCbo, getUndOrganicaSup, getEstados]).subscribe(
        (results) => {
          this.organos = results[0];
          this.tipoDocumento = results[1];
          this.regimenLaboral = results[2];
          this.unidadOrganicaCbo = results[3];
          this.unidadOrganicaSup = results[4];
          this.unidadOrganicaCboTemp = results[4];
          this.estados = results[5];
        },
        (err) => this.toastService.showToast(err, 'danger')
      );

  }
  get f() {
    return this.filterForm.controls;
  }

  initForm() {
    this.filterForm = this.fb.group({
      tipoOrganoId: '',
      unidadOrganicaSuperiorId: '',
      unidadOrganicaId: '',
      regimenLaboralId: '',
      datosServCivil: '',
      tipoDocumentoId: '',
      numeroDocumento: '',
      estadoId: '',
    });
  }
 
  files: File[] = [];

  onSelect(event) {
    console.log(event);
    this.files.push(...event.addedFiles);
  }

  onRemove(event) {
    console.log(event);
    this.files.splice(this.files.indexOf(event), 1);
  }

  downloadFile() {
    this.servidoresRepository.downloadExcel().subscribe(
      (res) => {
        const nameFile = `Plantilla de Servidores Civiles.xlsm`;
        const rutaFile = `data:application/vnd.ms-excel.sheet.macroEnabled.12;base64,${res}`;
        base64ToFilePromise(rutaFile, nameFile, '').then((file) =>
          FileSaver.saveAs(file)
        );
      },
      (err) => this.toastService.showToast(err.messages[0], 'danger')
    );
  }

  /***************************************************************/
  clear() {
    if (this.lstServCiviles.length !== 0) {
      this.lstServCiviles = [];
    }
    this.initForm();
    this.search();
    this.tipoOrganoChange(null);
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.lstServCiviles);
  }

  editServidor(item: ServidoresCiviles) {
    this.dataToEdit = item;
    //console.log("item:",item)
    //return false;
    // this.dialog.open(ModalServidorComponent);
    this.router.navigate(['editar'], {
      relativeTo: this.activatedRoute,
      queryParams: { id: item.personaId, uoId: item.organigramaId, detUoId: item.detUnidadOrganicaId, regimenId: item.regimenId }
    });
  }

  removeServidor(item: ServidoresCiviles) {
    this.dataToDelete = item;
    this.dialog.open(ModalEliminarComponent, {
      data: {
        bodyText: 'Se eliminará el siguiente registro ¿Estás seguro de realizar la siguiente acción?',
      },
    })
      .afterClosed()
      .subscribe(x => {
        if (x) {
          this.servidoresRepository.eliminarServidor(item.detUnidadOrganicaId)
            .subscribe(
              (deleted) => {
                if (deleted) {
                  this.toastService.showToast(
                    'Se realizó la eliminación exitosamente',
                    'success'
                  );
                  this.search();
                }
              },
              (err) => this.toastService.showToast(err, 'danger')
            );
        }
      })
      ;
  }

  getDataExport() {
    return null;
  }

  search() {
    this.searchMode = true;
    const body = this.filterForm.getRawValue();
    this.servidoresRepository.searchOrganigramaFilter(body)
    .subscribe(x => {
        this.lstServCiviles = x.map(p => {
         //   p.estadoRegistro === '1' ? p.colorEstado = '#00BFA6' : (p.estadoRegistro === '2' ? p.colorEstado = '#606D7D' : (p.estadoRegistro === '3' ? p.colorEstado = '#9a9a9a' : '#9a9a9a'));
          p.estado === 'ACTIVO' ? p.colorEstado = '#0d88bc' : p.colorEstado = '#9a9a9a';

            return{
              ...p
            };
        });
        /*this.formatColor();*/
        if (this.lstServCiviles.length === 0) {
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
    const getUndOrganicaSup = this.unidadOrganicaRepository.getUnidadOrganicaSup(tipoOrganoId);
    const getUndOrganicaCbo = this.unidadOrganicaRepository.getUnidadOrganicaCbo(tipoOrganoId);
    forkJoin([getUndOrganicaSup, getUndOrganicaCbo]).subscribe(
      (results) => {
        this.unidadOrganicaSup = results[0];
        this.unidadOrganicaCbo = results[1];
        this.llenarSubUO();
        this.f['unidadOrganicaSuperiorId'].setValue('');
        this.f['unidadOrganicaId'].setValue('');
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
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

  unidadOrganicaSuperiorChange(unidadOrganicaSuperiorId: number) {
    this.unidadOrganicaRepository.getUnidadOrganicaCbo(this.tipoOrganoId, unidadOrganicaSuperiorId)
      .subscribe(result => {
        this.unidadOrganicaCbo = result;
        this.f['unidadOrganicaId'].setValue('');
      });
  }


  openModalRegister() {
    this.dialog.open(ModalServidorComponent)
      .afterClosed()
      .subscribe(x => {
        if (x) {
          this.toastService.showToast(
            'Se realizó el registro exitosamente',
            'success'
          );
          this.search();
        }
      });
  }

  upload(file: any) {
    this.servidoresRepository
      .uploadFileMasivo(file)
      .subscribe(
        (res) => {

          if (res.status.success) {
           // if (res.payload.archivo.length === 0) {
             // this.openDialogOk();
            //} else {
              //this.openDialog(res.payload.archivo);
           // }
           this.toastService.showToast(res.payload.mensajeRespuesta, 'success');
          } else {
            this.toastService.showToast( "Ocurrió un error en el servidor", 'danger' );
            console.log(res.status.error.messages[0]);
          }
        },
        (err) => {
          this.toastService.showToast(err.messages[0], 'danger' );
          console.log(err);
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
      data: this.fileOrganoBase64,
    }).afterClosed().subscribe(item => {
      this.fileDropzone.removeFile();
      this.search();
    });
  }


}
