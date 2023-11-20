import {
  Component,
  OnInit,
  ViewChild,
  EventEmitter,
  Output,
  AfterViewInit,
} from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { MaestraParametro } from '../../../@data/model/maestra-parametro';
import { MaestraParametroRepository } from '../../../@domain/repository/maestra-parametro.repository';
import { forkJoin } from 'rxjs';
import { ServidorEntidad } from '../../../@data/model/servidoresentidad';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { base64ToFilePromise } from 'src/app/utils/converterFile';
import { ServidoresRepository } from '../../../@domain/repository/servidores.repository';
import * as FileSaver from 'file-saver';
import { MatDialog } from '@angular/material/dialog';
import { ParticipantesCiviles } from '../../../@data/model/servidores-civiles';
import { MatSort, Sort } from '@angular/material/sort';
import { sortDataTableComponent } from '../../../utils/general';
import { UnidadOrganicaRepository } from '../../../@domain/repository/unidad-organica.repository';
import { UnidadOrganicaCombo } from '../../../@data/model/unidadOrganicaCombo';
import { ModalServidorComponent } from './modal-servidor/modal-servidor.component';
import { ActivatedRoute, Router } from '@angular/router';
import { CicloService } from '../../../@data/services/ciclo.service';
import { AuthenticationRepository } from '../../../@domain/repository/authentication.repository';
import { TableColumn2 } from '../../@common-components/material-table-check-select/table-column';
import { MatTableDataSource } from '@angular/material/table';
import { SelectionModel } from '@angular/cdk/collections';
import { MatPaginator, MatPaginatorIntl } from '@angular/material/paginator';
import { getDutchPaginatorIntl } from '../../@common-components/material-table-checkbox/paginator-translate';
import { ModalActivarParticipanteComponent } from './modal-activar-participante/modal-activar-participante.component';
import { ModalExcluirParticipanteComponent } from './modal-excluir-participante/modal-excluir-participante.component';
import { ModalConfirmacionComponent } from './modal-confirmacion/modal-confirmacion.component';
import { ModalConfirmacionTodoBienComponent } from './modal-confirmacion-todo-bien/modal-confirmacion-todo-bien.component';
import { HttpEventType } from '@angular/common/http';
import { FDropzoneComponent } from './f-dropzone/f-dropzone.component';
import { Utils } from 'src/app/utils/utils';

const ELEMENT_DATA: ParticipantesCiviles[] = [];

@Component({
  selector: 'serv-talento-servidores',
  templateUrl: './servidores.component.html',
  styleUrls: ['./servidores.component.scss'],
  providers: [{ provide: MatPaginatorIntl, useValue: getDutchPaginatorIntl() }],
})
export class ServidoresComponent implements OnInit, AfterViewInit {
  @ViewChild(FDropzoneComponent) fDropzone: FDropzoneComponent;
  @ViewChild(MatPaginator) paginator: MatPaginator;
  @ViewChild(MatSort) sort: MatSort;
  paginationSizes: number[] = [10, 20, 50, 100];
  defaultPageSize = this.paginationSizes[0];
  displayedColumns: string[] = [
    'select',
    'numeroDocumento',
    'apellidosNombres',
    'siglaUO',
    'puesto',
    'tipoAsignacion',
    'segmentoId',
    'rolId',
    'estado',
    'estadoGME',
  ];
  dataSource = new MatTableDataSource<ParticipantesCiviles>(ELEMENT_DATA);
  selection = new SelectionModel<ParticipantesCiviles>(true, []);
  holderText =
    'N° Doc., Apellidos y nombres, UO, Puesto, Tipo de asignación, Segmento, Rol y Estado';
  isFilterable = true;
  isPageable = true;

  public model = null;
  searchText: string = '';

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

  servidores: ServidorEntidad[] = [];

  /**************************/
  searchMode = false;
  ordersTableColumns: TableColumn2[];
  filterForm: FormGroup = null;
  tipoOrganoId: number = 0;
  /***************************/
  frm: FormGroup = null;
  profile = this.authenticationService.getCurrentUserValue;
  ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
  cicloDefaultDesc: string;
  cicloDefault: number;
  lstParticipantesCiviles: ParticipantesCiviles[] = [];
  dataToEdit: ParticipantesCiviles = null;
  dataToDelete: ParticipantesCiviles = null;
  segmentos: MaestraParametro[] = [];
  roles: MaestraParametro[] = [];
  rol;
  estadoTipo: MaestraParametro[] = [];
  listRolesSecundario: MaestraParametro[] = [];
  unidadOrganicaCboTemp: UnidadOrganicaCombo[];
  isDisabled:boolean = false;
  isMonitoreo:boolean = false;

  /*****************************/
  // displayedColumns: string[] = ['select', 'position', 'name', 'weight', 'symbol'];
  // public tableDataSource = new MatTableDataSource<ParticipantesCiviles>(this.lstParticipantesCiviles);
  // public displayedColumns: string[];
  // dataSource = new MatTableDataSource<PeriodicElement>(ELEMENT_DATA);
  // selection = new SelectionModel<PeriodicElement>(true, []);
  /*****************************/
  constructor(
    private fb: FormBuilder,
    private maeParametroRepository: MaestraParametroRepository,
    private servidoresRepository: ServidoresRepository,
    private toastService: ToastService,
    private UnidadOrganicaRepository: UnidadOrganicaRepository,
    public dialog: MatDialog,
    private router: Router,
    private activatedRoute: ActivatedRoute,
    private cicloService: CicloService,
    private authenticationService: AuthenticationRepository
  ) {}

  ngOnInit(): void {
    this.setCiclo();
    this.initForm();
    if (this.ciclo?.cicloId) {
      if (this.ciclo?.estadoCicloId === 1 || this.ciclo?.estadoCicloId === 2) {
        this.loadCombox();
        this.search();
      } else {
        this.toastService.showToast(
          'El ciclo seleccionado no se encuentra vigente',
          'danger'
        );
        this.isDisabled = true;
      }
    } else {
      this.organos = [];
      this.tipoDocumento = [];
      this.regimenLaboral = [];
      this.unidadOrganicaCbo = [];
      this.unidadOrganicaSup = [];
      this.estados = [];
      this.segmentos = [];
      this.roles = [];
      this.estadoTipo = [];
      this.toastService.showToast(
        'No se tiene seleccionado ningún ciclo',
        'danger'
      );
    }

    this.Monitoreo();
  }

  Monitoreo() {
    let monitoreo = Utils.obtenerSiEsPantallMonitoreo();
    if(monitoreo) {
       this.isMonitoreo = true;
    }
  }


  loadCombox() {
    const getOrganos = this.maeParametroRepository.getMaestraParametro(
      'TIPO_NATURALEZA'
    );
    const getTipoDocumento = this.servidoresRepository.getTiposDocumento();
    const getRegimenLaboral = this.maeParametroRepository.getMaestraParametro(
      'TIPO_REGIMEN_LABORAL'
    );
    const getUndOrganicaCbo = this.UnidadOrganicaRepository.getUnidadOrganicaCbo();
    const getUndOrganicaSup = this.UnidadOrganicaRepository.getUnidadOrganicaSup();
    const getEstados = this.maeParametroRepository.getMaestraParametro(
      'ESTADO_SERVIDOR_CIVIL_GDR'
    );
    const getSegmento = this.maeParametroRepository.getMaestraParametro(
      'SEGMENTO_GDR'
    );
    const getRol = this.maeParametroRepository.getMaestraParametro('TIPO_ROL');
    const getEstadoTipo = this.maeParametroRepository.getMaestraParametro(
      'ESTADO_TIPO_SERVIDOR_CIVIL_GDR'
    );
    this.searchMode = false;
    forkJoin([
      getOrganos,
      getTipoDocumento,
      getRegimenLaboral,
      getUndOrganicaCbo,
      getUndOrganicaSup,
      getEstados,
      getSegmento,
      getRol,
      getEstadoTipo,
    ]).subscribe(
      (results) => {
        console.log(results);
        this.organos = results[0];
        this.tipoDocumento = results[1];
        this.regimenLaboral = results[2];
        this.unidadOrganicaCbo = results[3];
        this.unidadOrganicaSup = results[4];
        this.unidadOrganicaCboTemp = results[4];


console.log(" this.unidadOrganicaCboTemp :", this.unidadOrganicaCboTemp)

        this.estados = results[5];
        this.segmentos = results[6];
        this.roles = results[7];
        this.estadoTipo = results[8];
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }
 
  setCiclo() {
    if (this.ciclo.length !== 0 && this.ciclo) {
      this.cicloDefaultDesc = ' - ' + this.ciclo.anio;
      this.cicloDefault = this.ciclo.cicloId;
    } else {
      this.cicloDefaultDesc = '';
    }
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
      estadoSerCivGdrId: '',
    });

    this.frm = this.fb.group({
      segmento: '',
    });
  }

  files: File[] = [];

  onSelect(event) {
    this.files.push(...event.addedFiles);
  }

  onRemove(event) {
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
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  /***************************************************************/
  clear() {
    if (this.lstParticipantesCiviles.length !== 0) {
      this.lstParticipantesCiviles = [];
    }
    this.initForm();
    this.search();
    this.tipoOrganoChange(null);
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.lstParticipantesCiviles);
  }
 
  search() {
    this.searchMode = true;
    const body = this.filterForm.getRawValue();
 
    this.servidoresRepository.listarParticipantesServidoresCiviles(body).subscribe(
      (res) => {
        this.lstParticipantesCiviles = res
        .map((p) => {
          p.estado === 'Activo'
            ? (p.colorEstado = '#0d88bc')
            : (p.colorEstado = '#9a9a9a');
          return {
            ...p,
          };
        });
        if (this.lstParticipantesCiviles.length === 0) {
          this.toastService.showToast(
            'No se encontraron resultados',
            'primary'
          );
        }
        this.setTableDataSource(this.lstParticipantesCiviles);
      },
      (err) => {
        this.toastService.showToast(err.message, 'danger');
      }
    );
  }

  tipoOrganoChange(tipoOrganoId: number) {
    this.tipoOrganoId = tipoOrganoId;
    const getUndOrganicaSup = this.UnidadOrganicaRepository.getUnidadOrganicaSup(
      tipoOrganoId
    );
    const getUndOrganicaCbo = this.UnidadOrganicaRepository.getUnidadOrganicaCbo(
      tipoOrganoId
    );
 
    forkJoin([getUndOrganicaSup, getUndOrganicaCbo]).subscribe(
      (results) => {
        this.unidadOrganicaSup = null;
        this.unidadOrganicaCbo = results[1];
        this.llenarSubUO();
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  llenarSubUO () {
    var idSuperiores = [];
    console.log(" this.unidadOrganicaCbo:", this.unidadOrganicaCbo)
    this.unidadOrganicaCbo.forEach(function (element, index, array) {
      idSuperiores.push(element.uoSupId); 
   });

   console.log("idSuperiores",idSuperiores)

   let ArrayId = Array.from(new Set(idSuperiores));;
   var unidadSup: UnidadOrganicaCombo[]=[];
   console.log("ArrayId 3: ",ArrayId)
   console.log("ArrayId 3: ",ArrayId.length)

    for (var i = 0; i< ArrayId.length ; i++) {

      this.unidadOrganicaCboTemp.filter(unidad => {

     console.log("identificador:",i)
     if(unidad.id === ArrayId[i]) {
      console.log("unidad.id:",unidad.id)
      console.log("idSuperiores[i]:",ArrayId[i])
      console.log("unidad: ",unidad)

      unidadSup.push(unidad);
     }

    }
    );
    }
 
    this.unidadOrganicaSup = [...unidadSup];

   console.log("unidadOrganicaSup tem: ",this.unidadOrganicaSup)

    unidadSup = [];
    idSuperiores = [];
  }
 
  unidadOrganicaSuperiorChange(unidadOrganicaSuperiorId: number) {
    this.UnidadOrganicaRepository.getUnidadOrganicaCbo(
      this.tipoOrganoId,
      unidadOrganicaSuperiorId
    ).subscribe((result) => {
      this.unidadOrganicaCbo = result;
      this.f['unidadOrganicaId'].setValue('');
    });
  }

  openModalRegister() {
    this.dialog
      .open(ModalServidorComponent)
      .afterClosed()
      .subscribe((x) => {
        if (x) {
          this.toastService.showToast(
            'Se realizó el registro exitosamente',
            'success'
          );
          this.search();
        }
      });
  }

  onChangeSegmento(events, item, i) {
    const element = { segmento: events, item, i };
    if (this.cicloDefault && element) {
      const body = {
        trace: {
          traceId: 'string',
        },
        payload: {
          entidadId: this.profile.entidadId,
          segmentoId: element.segmento,
          detUoId: element.item.detUnidadOrganicaId,
          personaId: element.item.personaId,
          cicloId: this.cicloDefault,
        },
      };
      this.servidoresRepository.updateSegmento(body).subscribe(
        (result) => {
          const getItem = {
            segmentoId: result.payload.segmentoId,
            i: element.i,
            esJefeUo: element.item.esJefeUo,
          };
          if (result.status.success) {
            this.searchText = '';
            this.searchAndRol(getItem);
            this.toastService.showToast('Se actualizó con exito', 'primary');
          } else {
            this.toastService.showToast('Error, no se actualizó', 'danger');
          }
        },
        (err) => {
          this.toastService.showToast(err, 'danger');
        }
      );
    } else {
      this.toastService.showToast('Registrar un ciclo', 'danger');
    }
  }

  searchAndRol(getItem) {
    this.searchMode = true;
    const body = this.filterForm.getRawValue();
    this.servidoresRepository.listarParticipantesServidoresCiviles(body).subscribe(
      (res) => {
        console.log (res);
        this.lstParticipantesCiviles = res.map((p) => {
          p.estado === 'Activo'
            ? (p.colorEstado = '#0d88bc')
            : (p.colorEstado = '#9a9a9a');

          return {
            ...p,
          };
        });
        if (this.lstParticipantesCiviles.length === 0) {
          this.toastService.showToast(
            'No se encontraron resultados',
            'primary'
          );
        } else {
          this.rolSecundario(getItem);
        }
        this.selection.clear();
      },
      (err) => {
        this.toastService.showToast(err.message, 'danger');
      }
    );
  }

  rolSecundario(getItem) {
    this.listRolesSecundario = [];
    this.servidoresRepository
      .listarRolesPorSegmento(getItem.segmentoId, getItem.esJefeUo)
      .subscribe((item) => {
        console.log (item);
        let elementRol: any[] = [];
        this.lstParticipantesCiviles[getItem.i].roles = [];
        if (item.length > 0) {
          item.forEach((it) => {
            this.rol = null;
            this.rol = this.roles.find((val) => val.codigoNumero === it.rolId);
            if (this.rol) {
              elementRol.push(this.rol);
            }
          });
          this.lstParticipantesCiviles[getItem.i].roles = elementRol;
          this.setTableDataSource(this.lstParticipantesCiviles);
        } else {
          this.search();
        }
      });
  }

  onChangeRol(events, item, i) {
    const element = { events, item, i };
    if (this.cicloDefault && element) {
      const body = {
        trace: {
          traceId: 'string',
        },
        payload: {
          entidadId: this.profile.entidadId,
          segmentoId: element.item.segmentoId,
          detUoId: element.item.detUnidadOrganicaId,
          personaId: element.item.personaId,
          cicloId: this.cicloDefault,
          rolId: element.events,
        },
      };
      this.servidoresRepository.updateSegmento(body).subscribe(
        (result) => {
          if (result.status.success) {
            this.toastService.showToast('Se actualizó con exito', 'primary');
          } else {
            this.toastService.showToast('Error, no se actualizó', 'danger');
          }
          this.search();
        },
        (err) => {
          this.toastService.showToast(err, 'danger');
        }
      );
    } else {
      this.search();
      this.toastService.showToast('Registrar un ciclo', 'danger');
    }
  }

  /*********************************************/

  setTableDataSource(data: any) {
    console.log(data);
    this.dataSource = new MatTableDataSource<any>(data);
    this.dataSource.paginator = this.paginator;
    this.dataSource.sort = this.sort;
  }

  ngAfterViewInit(): void {
    this.dataSource.paginator = this.paginator;
  }

  applyFilter(event: Event) {
    const filterValue = (event.target as HTMLInputElement).value;
    this.dataSource.filter = filterValue.trim().toLowerCase();
    if (this.dataSource.paginator) {
      this.dataSource.paginator.firstPage();
    }
  }

  masterToggle() {
    if (this.isAllSelected()) {
      this.selection.clear();
      return;
    }
    this.selection.select(...this.dataSource._pageData(this.dataSource.data));
  }

  checkboxLabel(row?: ParticipantesCiviles): string {
    if (!row) {
      return `${this.isAllSelected() ? 'deselect' : 'select'} all`;
    }
    return `${this.selection.isSelected(row) ? 'deselect' : 'select'} row ${
      row.estadoId + 1
    }`;
  }

  isAllSelected() {
    if (this.dataSource.data.length === this.selection.selected.length) {
      const numSelected = this.selection.selected.length;
      const numRows = this.selection.selected.length;
      return numSelected === numRows;
    } else {
      const numSelected = this.selection.selected.length;
      const numRows = this.dataSource.paginator.pageSize;
      return numSelected === numRows;
    }
  }

  activar() {
    if (this.selection.selected.length > 0) {
      let estadoInactivo = this.estados.find(
        (estado) => estado.codigoNumero === 4
      );
      let estadoActivo = this.estados.find(
        (estado) => estado.codigoNumero === 1
      );
      let activar = this.selection.selected.filter(
        (it) => it.estadoSerCivGdrId === estadoInactivo.codigoNumero
      );

      console.log("activar:",activar)
      console.log("this.selection.selected:",this.selection.selected)

      let rol = this.selection.selected.filter((it2) => it2.rolId !== null);
      let segmento = this.selection.selected.filter(
        (it3) => it3.segmentoId !== null
      );

      if (this.selection.selected.length === 1) {
        if (activar.length === this.selection.selected.length) {
          if (
            rol.length === this.selection.selected.length &&
            segmento.length === this.selection.selected.length
          ) {
            this.openModalActivar(estadoActivo);
          } else {
            this.toastService.showToast(
              'Para activar un servidor/a debe registrar el segmento y rol',
              'danger'
            );
          }
        } else {
          this.toastService.showToast(
            'El/la servidor/a ya se encuentra activo',
            'danger'
          );
        }
      } else {
        if (activar.length === this.selection.selected.length) {
          if (
            rol.length === this.selection.selected.length &&
            segmento.length === this.selection.selected.length
          ) {
            this.openModalActivar(estadoActivo);
          } else {
            this.toastService.showToast(
              'Para activar un servidor/a debe registrar el segmento y rol',
              'danger'
            );
          }
        } 
        /*lse {
          this.toastService.showToast(
            'Uno o varios de los/las servidores/as ya se encuentra activo',
            'danger'
          );
        }*/
      }
    } else {
      this.toastService.showToast('Selecciona un servidor civil', 'danger');
    }
  }

  openModalActivar(estadoActivo) {
    this.dialog
      .open(ModalActivarParticipanteComponent)
      .afterClosed()
      .subscribe((x) => {
        if (x) {
          this.selection.selected.forEach((element) => {
            const body = {
              trace: {
                traceId: 'string',
              },
              payload: {
                entidadId: this.profile.entidadId,
                segmentoId: element.segmentoId,
                detUoId: element.detUnidadOrganicaId,
                personaId: element.personaId,
                cicloId: this.cicloDefault,
                rolId: element.rolId,
                estadoSerCivGdrId: estadoActivo.codigoNumero,
              },
            }; 
            this.servidoresRepository.activarParticipante(body).subscribe(
              (result) => {
                if (result.status.success) {
                  this.toastService.showToast(
                    'Se actualizó con exito',
                    'primary'
                  );
                } else {
                  this.toastService.showToast(
                    'Error, no se actualizó',
                    'danger'
                  );
                }
                this.selection.clear();
                this.search();
              },
              (err) => {
                this.toastService.showToast(err, 'danger');
              }
            );
          });
        }
      });
  }

  excluir() {
    if (this.selection.selected.length > 0) {
      let estadoActivo = this.estados.find(
        (estado) => estado.codigoNumero === 1
      );
      let excluir = this.selection.selected.filter(
        (it) => it.estadoSerCivGdrId === estadoActivo.codigoNumero
      );
      if (excluir.length === this.selection.selected.length) {
        let estados = this.estadoTipo.filter(
          (item3) => item3.codigoNumero !== 1
        );
        this.dialog
          .open(ModalExcluirParticipanteComponent, {
            data: { estados: estados, items: this.selection },
          })
          .afterClosed()
          .subscribe((x) => {
            if (x) {
              let estadoId = this.estadoTipo.find(
                (estado) => estado.codigoNumero === x
              ).codigoNumero;
              this.selection.selected.forEach((element) => {
                const body = {
                  trace: {
                    traceId: 'string',
                  },
                  payload: {
                    entidadId: this.profile.entidadId,
                    segmentoId: element.segmentoId,
                    detUoId: element.detUnidadOrganicaId,
                    personaId: element.personaId,
                    cicloId: this.cicloDefault,
                    rolId: element.rolId,
                    estadoSerCivGdrId: estadoId,
                  },
                };
                return this.servidoresRepository.updateSegmento(body).subscribe(
                  (response) => {
                    if (response.status.success) {
                      this.toastService.showToast(
                        'Se actualizó con exito',
                        'primary'
                      );
                    } else {
                      this.toastService.showToast(
                        'Error, no se actualizó',
                        'danger'
                      );
                    }
                    this.selection.clear();
                    this.search();
                  },
                  (err) => {
                    this.toastService.showToast(err, 'danger');
                  }
                );
              });
            }
          });
      } else {
        if (this.selection.selected.length === 1) {
          this.toastService.showToast(
            'El/la servidor/a ya se encuentra inactivo',
            'danger'
          );
        } else {
          this.toastService.showToast(
            'Uno o varios de los/las servidores/as ya se encuentra inactivo',
            'danger'
          );
        }
      }
    } else {
      this.toastService.showToast('Selecciona un servidor civil', 'danger');
    }
  }

  uploadFile(file: any) {
    this.servidoresRepository
      .uploadFileMasivo(file, this.ciclo.cicloId)
      .subscribe(
        (res) => {
          if (res.type === HttpEventType.Response) {
            if (res.body.status.success) {
              if (res.body.payload.archivo.length === 0) {
                this.openDialogOk(file);
              } else {
                this.openDialog(res.body.payload.archivo);
              }
            } else {
              this.toastService.showToast(
                'Ocurrió un error en el servidor',
                'danger'
              );
              console.log(res.body.status.error.messages[0]);
              this.fDropzone.removeFile();
              this.search();
            }
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

  openDialog(archivo: any[]) {
    this.dialog
      .open(ModalConfirmacionComponent, {
        disableClose: true,
        data: archivo,
      })
      .afterClosed()
      .subscribe((item) => {
        this.fDropzone.removeFile();
        this.search();
      });
  }

  openDialogOk(file: any) {
    this.dialog
      .open(ModalConfirmacionTodoBienComponent, {
        disableClose: true,
        data: file,
      })
      .afterClosed()
      .subscribe((item) => {
        this.fDropzone.removeFile();
        this.search();
      });
  }

  getSegmentos (esJefeUo: string) {
    let returned: any [] = this.segmentos;

    if (esJefeUo === 'S') {
      returned = this.segmentos.filter ((item: any) => {
        return item.valorTexto === 'Funcionario' || item.valorTexto === 'Directivo';
      });
    } else {
      returned = this.segmentos.filter ((item: any) => {
        return item.valorTexto !== 'Funcionario' && item.valorTexto !== 'Directivo';
      });
    }

    return returned;
  }
}
