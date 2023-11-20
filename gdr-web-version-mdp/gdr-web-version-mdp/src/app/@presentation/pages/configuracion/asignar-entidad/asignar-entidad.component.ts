import {
  Component,
  Inject,
  OnInit, ViewChild
} from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { forkJoin } from 'rxjs';
import { DatePipe } from '@angular/common';
import { MaestraParametroRepository } from '../../../../@domain/repository/maestra-parametro.repository';
import { MaestraParametro } from '../../../../@data/model/maestra-parametro';
import { ToastService } from '../../../@common-components/toast';
import { MatDialog } from '@angular/material/dialog';
import { ImplementacionRepository } from '../../../../@domain/repository/implementacion.repository';
import { ParameterRepository } from 'src/app/@domain/repository/parameter.repository';
import { CicloRepository } from '../../../../@domain/repository/ciclo.repository';
import { Router } from '@angular/router';
import { TableColumn } from '../../../@common-components/material-table/table-column';
import { Const } from '../../../../@data/services/const';
import { EntidadGestor } from '../../../../@data/model/entidadGestor';
import { ConfiguracionRepository } from '../../../../@domain/repository/configuracion.repository';
import { MaterialTableComponent } from '../../../@common-components/material-table/material-table.component';

@Component({
  selector: 'serv-talento-asignar-entidad',
  templateUrl: './asignar-entidad.component.html',
  styleUrls: ['./asignar-entidad.component.scss']
})

export class AsignarEntidadComponent implements OnInit {

  @ViewChild('table') tableSelect: MaterialTableComponent;
  [x: string]: any;
  searchMode = false;
  const = Const;
  tableColumns: TableColumn[];
  entidadId:any;

  private usuarioRector = null;

  constructor(
    private fb: FormBuilder,
    private dialog: MatDialog,
    private maeParametroRepository: MaestraParametroRepository,
    private parameterRepository: ParameterRepository,
    private router: Router,
    private toastService: ToastService,
    private cicloRepository: CicloRepository,
    protected ref: MatDialogRef<AsignarEntidadComponent>,
    private datePipe: DatePipe,
    private implementacionRepository: ImplementacionRepository,
    private configuracionRepository: ConfiguracionRepository,

    @Inject(MAT_DIALOG_DATA) public data: AsignarEntidadComponent,
  ) {
      this.usuarioRector = data;
   }

    frm: FormGroup = null;
    cicloDefaultDesc;
    cicloDefault;
    sector: MaestraParametro[];
    nivel: MaestraParametro[];
    cicloFilter: any;
    tipoEntidad: MaestraParametro[];
    fecha = new Date();
    anioFilter = null;
    listaEntidadesFiltradas = [];
    listaEntidades = [];
    lstImplementacion = [];
    dataR: any[] = [];
    listBody: null;
    lstUsuarioRector: EntidadGestor[] = [];
    titleBtn = 'Aceptar';

  get f() {
    return this.frm.controls;
  }

  files: File[] = [];

  addEntidades(data) {
  }

  clear() {
    this.loadCombox();
    this.initForm();
  }

  dismiss(success: boolean) {
    this.ref.close(success);
  }

  ngOnInit(): void {
    this.anioFilter = this.fecha.getFullYear();
    this.loadCombox();
    this.initForm();
    this.initializeColumns();
  }

  initializeColumns() {
    this.tableColumns = [
      { name: 'Nro', dataKey: 'nro', position: 'left', isSortable: true, innerHtml: true, width: '10%' },
      { name: 'Entidad', dataKey: 'siglaNombreEntidad', position: 'left', isSortable: true, innerHtml: true, width: '75%' },
    ];

  }

  loadCombox() {
    const getNivel = this.parameterRepository.getNiveles();
    const getSector = this.parameterRepository.getSectores();
    const getTipoFilter = this.parameterRepository.getTipoEntidad();
    const getListEntidad = this.implementacionRepository.getListEntidades();
   
    const getCicloFilter = this.cicloRepository.getCiclosFilter();
    const getListImpl = this.implementacionRepository.getListImplementacion(this.anioFilter);
    const getListEntiRector = this.implementacionRepository.getListadoEntidadRector(this.usuarioRector.rector.rectorId, 1);
    forkJoin([ getNivel , getSector ,getTipoFilter , getListEntidad, getListEntiRector, getListImpl, getCicloFilter,
     ]).subscribe(
      (results) => {
        this.nivel = results[0];
        this.sector = results[1];
        this.tipoEntidad = results[2];
        this.listaEntidades = results[3];
        this.lstUsuarioRector = results[4];
        this.lstImplementacion = results[5];
        this.cicloFilter = results[6];
        // 
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  initForm() {
    this.frm = this.fb.group({
      nivel: '',
      sector: '',
      tipoEntidad: '',
      entidad: '',
    });
  }

  onChangeEntidad($event: any) {
    let valor = $event.target.value.trim().toLowerCase();
    this.listaEntidadesFiltradas = this.listaEntidades.filter(x => x.siglaDescripcion.toLowerCase().includes(valor));
  }

  onSelectionChangeEntidad(idEntidad: number) {

    console.log("idEntidad:",idEntidad);
    console.log("listaEntidades:",this.listaEntidades);


    let seleccionado = this.listaEntidades.find(x => x.entidadId === idEntidad);

    console.log("seleccionado:",seleccionado);

    if (seleccionado) {
      this.entidadId = null;
      this.frm.get('entidad').setValue(seleccionado.descripcionEntidad);
      this.entidadId = idEntidad
    }
  }

  search() {
    let nivel, sector, tipoEntPub, entidad = null;
    if ( this.frm.value.nivel ) { nivel = this.frm.value.nivel; }
    if ( this.frm.value.sector ) { sector = this.frm.value.sector; }
    if ( this.frm.value.tipoEntidad ) { tipoEntPub = this.frm.value.tipoEntidad; }
    if ( this.frm.value.entidad ) {
      let val = this.frm.value.entidad;
      let entidadBusca = null;
      if ( val ) {
        console.log("entidad selccionada: ",this.entidadId)
        console.log("listaEntidades : ",this.listaEntidades)

        entidadBusca = this.listaEntidades.find(item => item.entidadId === this.entidadId );
        console.log("entidad", entidadBusca )
        entidad = entidadBusca.descripcionEntidad;
      } else {
        entidad = this.frm.value.entidad;
      }
    }
    this.implementacionRepository.getListadoEntidadRector(
      this.usuarioRector.rector.rectorId, 2, nivel, sector, tipoEntPub, entidad)
      .subscribe(result => {
        this.lstUsuarioRector = result;
        let set                 = new Set(this.lstUsuarioRector)
        // let arrSinDuplicaciones = Array.from(set).map( JSON.parse );
        let letrasUnicas = [...new Set(this.lstUsuarioRector)];

console.log("letrasUnicas:",letrasUnicas);
        console.log( "lstUsuarioRector:",this.lstUsuarioRector );
     
    });
  }

  activar() {
    let seleccionados = this.tableSelect.selection.selected;
    let noSeleccionados = this.tableSelect.getNotSelected();
    let idRector = this.usuarioRector.rector.rectorId;
    let asigRecSeleccionado = null;
    let asigRecNoSeleccionado = null;
    let arrayEntidad: any[] = [];
    if ( seleccionados.length ) {
      seleccionados.forEach( item => {
        if ( item.flagAsociado === null  || item.flagAsociado === '0') {
          item.flagCambio = '1';
          asigRecSeleccionado = {
            rolEntidadId: item.rolEntidadId,
            rectorId: idRector,
            entidadId: item.entidadId,
            descripcionEntidad: item.descripcionEntidad,
            siglaEntidad: item.siglaEntidad,
            flagAsociado: item.flagCambio
          };
          arrayEntidad.push(asigRecSeleccionado);
        }
      });
    }
    if ( noSeleccionados.length  ) {
      noSeleccionados.forEach( item => {
        if ( item.flagAsociado === '1' ) {
          item.flagCambio = '0';
          asigRecNoSeleccionado = {
            rolEntidadId: item.rolEntidadId,
            rectorId: idRector,
            entidadId: item.entidadId,
            descripcionEntidad: item.descripcionEntidad,
            siglaEntidad: item.siglaEntidad,
            flagAsociado: item.flagCambio
          };
          arrayEntidad.push(asigRecNoSeleccionado);
        }
      });
    }
    let body = {
      trace: {
        traceId: "string"
      },
      payload: {
      listaAsignacionRector: arrayEntidad
      }
    };
    if ( arrayEntidad.length > 0 ) {
      this.configuracionRepository.asignaRectorEntidad(body)
        .subscribe(response => {
          if ( response.status.success ) {
            this.toastService.showToast('Se realizó la asignación de manera exitosa', 'success', 'Entidad(es) asignadas');
            this.dismiss(true);
          } else {
            this.toastService.showToast(response.status.error.messages[0].message, 'danger');
          }
        });
    } else {
      this.toastService.showToast('Realizar la búsqueda y selección de una entidad', 'info');
    }

  }
}
