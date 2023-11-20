import { Component, OnInit, ViewChild } from '@angular/core';
import { FormControl, FormGroup } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import FileSaver from 'file-saver';
import { base64ToFilePromise } from 'src/app/utils/converterFile';
import { TableColumn } from '../../@common-components/material-table/table-column';
import { ToastService } from '../../@common-components/toast';
import { ModalRegistrarComponent } from './modal-registrar/modal-registrar.component';
import { UnidadOrganicaRepository } from 'src/app/@domain/repository/unidad-organica.repository';
import { PuestoRepository } from 'src/app/@domain/repository/puesto.repository';
import { UnidadOrganicaCombo } from 'src/app/@data/model/unidadOrganicaCombo';
import { ModalEliminarComponent } from './modal-eliminar/modal-eliminar.component';
import { ModalConfirmacionTodoBienComponent } from '../../@common-components/modal-confirmacion-todo-bien/modal-confirmacion-todo-bien.component';
import { PuestoModel } from 'src/app/@data/model/puesto';
import { FileDropzoneComponent } from '../../@common-components/file-dropzone/file-dropzone.component';
import { ModalConfirmacionObsComponent } from './modal-confirmacion-obs/modal-confirmacion-obs.component';

@Component({
  selector: 'serv-talento-puestos',
  templateUrl: './puestos.component.html',
  styleUrls: ['./puestos.component.scss']
})
export class PuestosComponent implements OnInit {
  @ViewChild(FileDropzoneComponent) fileDropzone: FileDropzoneComponent;
  holderText = "Buscar por Órgano, Nombre de puesto, Jefe inmediato ...";
  searchMode = false;
  dataToEdit: PuestoModel = null;
  tabResoluciones = false;
  frm: FormGroup = null;
  listUnidadesOrganicas: UnidadOrganicaCombo[] = [];
  puestos: PuestoModel[] = [];
  ordersTableColumns: TableColumn[];
  listPuestosCbo: any[] = [];
  fileOrganoBase64 = '';
  seleccionado = 'Seleccione';
  extension = 'xlsx';
  constructor (
    private dialog: MatDialog,
    private unidadOrganicaRepository: UnidadOrganicaRepository,
    private puestoService: PuestoRepository,
    private toastService: ToastService,

  ) { }

  ngOnInit (): void {
    this.initForm();
    this.initializeColumns();
    this.loadCombox();
    this.searchPuesto();
  }

  loadCombox() {
    this.unidadOrganicaRepository.getUnidadOrganicaCbo()
      .subscribe(
        (results) => {
          this.listUnidadesOrganicas = results;
        },
        (error) => { console.log(error); }
      );
  }

  searchPuesto() {
    this.searchMode = true;
    const body = this.frm.getRawValue();
    this.puestoService.searchPuestos(body).subscribe(
      (results) => {
        console.log("Data",results);
        this.puestos = results;
        if (results.length === 0) {
          this.toastService.showToast(
            'No se encontraron resultados',
            'primary'
          );
        }
      },
      (error) => {
        this.toastService.showToast(error + ', No se cargo la información correctamente', 'danger');
      }
    );
  }

  unidaOrganicaChange(event) {
  console.log(event);

    if (event) {
      this.puestoService.searchPuestos({
        unidadOrganicaID: event,
        puestoId: '',
        esJefe: '',
        nombrePuesto: ''
      }).subscribe(
        (results) => {
          console.log(results);
          this.listPuestosCbo = results;
          if (results.length !== 0 || results.length === 0 ) {
            this.frm.get('puestoId').enable();
          }
        },
        (error) => {
          this.frm.get('puestoId').disable();
          this.toastService.showToast(error + ', No se cargo la información correctamente', 'danger');

        }
      );
    } else {
      this.frm.get('puestoId').setValue("");
      this.frm.get('puestoId').disable();

    }

  }

  initForm() {
    this.frm = new FormGroup({
      unidadOrganicaID: new FormControl(''),
      puestoId: new FormControl({value: '', disabled: true}),
      esJefe: new FormControl(''),
      nombrePuesto: new FormControl('')
    });
  }

  initializeColumns() {
    this.ordersTableColumns = [
      { name: 'Órgano / UO / Sub UO', dataKey: 'unidadOrganica', position: 'left', isSortable: true },
      { name: 'Nombre de puesto', dataKey: 'nombrePuesto', position: 'left', isSortable: true },
      { name: 'Es jefe', dataKey: 'esJefe', position: 'left', isSortable: true },
      { name: 'Jefe inmediato', dataKey: 'jefeInmediato', position: 'left', isSortable: true },
      { name: 'N de trabajadores', dataKey: 'nroTrabajadores', position: 'left', isSortable: true },
    ];
  }

  onTabChanged(index: number) {
    this.tabResoluciones = index > 0;
    if (index === 0) {
    } else if (index === 1) {
      console.log(index);
    }
  }


  clear() {
    if (this.puestos.length !== 0) {
      this.puestos = [];
    }

  this.frm.reset();
  this.frm.get('puestoId').disable();
  this.searchPuesto();

  }


  sortData($event) {}

  editUO(item: PuestoModel) {
    this.dataToEdit = item;
    this.openModalRegister(false);
  }

  openModalRegister(createMode: boolean = true) {
    const registerDialog = this.dialog.open(ModalRegistrarComponent, {
      data: {
       createMode,
       dataToEdit: this.dataToEdit
      },
    });
    registerDialog.afterClosed().subscribe((res) => {
      this.dataToEdit = null;
      if (res) {
        this.searchPuesto();
      }
    });
  }

  removeGO(item: any) {
    const confirmModal = this.dialog.open(ModalEliminarComponent, {
      data: {
        bodyText: 'El siguiente puesto a eliminar cuenta con servidores asignados, para proceder necesitará estar sin usuarios asignados a este.',
        textCancel: 'Ir a Servidores Civiles',
        link: '/pages/servidoresCiviles'
      },
    });
    confirmModal.afterClosed().subscribe((res) => {
      if (res === true) {
        this.puestoService.deleteGO(item?.puestoId).subscribe((x) => {
          if ( x === true) {
            this.toastService.showToast(
              'Se realizó la eliminación exitosamente',
              'success'
            );
            this.initForm();
            this.searchPuesto();
          } else {
            this.toastService.showToast(
              'El puesto tiene dependencias',
              'danger'
            );
          }
        },
          (err) => {
            this.toastService.showToast(err, 'danger');
          }
        );
      }
    });
  }


  downloadFormatPuesto() {
    this.puestoService.downloadFormatPuesto().subscribe(
      (res) => {
        const nameFile = `Plantilla de Puestos.xlsx`;
        const rutaFile = `data:application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;base64,${res}`;
        base64ToFilePromise(rutaFile, nameFile, '').then((file) =>
          FileSaver.saveAs(file)
        );
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  upload(file: any) {
    this.puestoService
      .uploadFileMasivo(file)
      .subscribe(
        (res) => {
          if (res === true) {
            this.openDialogOk();

          } else {
            this.openDialog(res.archivo, this.extension);
          }
        },
        (err) => {
          this.toastService.showToast(
            'Ocurrió un error en el servidor',
            'danger'
          );
        }
      );
  }

  openDialog(archivo: any[], extension: string) {
    let data = { archivo: archivo, extension: extension };
    this.dialog.open(ModalConfirmacionObsComponent, {
      disableClose: true,
      data: data,
    }).afterClosed().subscribe(item => {
      this.fileDropzone.removeFile();
      this.searchPuesto();
    });
  }

  openDialogOk() {
    this.dialog.open(ModalConfirmacionTodoBienComponent, {
      disableClose: true,
      data: this.fileOrganoBase64
    }).afterClosed().subscribe(item => {
      this.fileDropzone.removeFile();
      this.searchPuesto();
    });
  }


}
