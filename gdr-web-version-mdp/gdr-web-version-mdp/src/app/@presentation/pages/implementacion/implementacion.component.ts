import { Component, OnInit } from '@angular/core';
import { MaestraParametro } from '../../../@data/model/maestra-parametro';
import { MaestraParametroRepository } from '../../../@domain/repository/maestra-parametro.repository';
import { BehaviorSubject, forkJoin, Observable } from 'rxjs';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { ActivatedRoute, Router} from '@angular/router';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { Const } from '../../../@data/services/const';
import { ParameterRepository } from 'src/app/@domain/repository/parameter.repository';
import { CicloRepository } from '../../../@domain/repository/ciclo.repository';
import { MatDialog } from '@angular/material/dialog';
import { ImplementacionRepository } from '../../../@domain/repository/implementacion.repository';
import { TableColumn } from '../../@common-components/material-table/table-column';
import { User } from 'src/app/@data/model/user';
import { AuthenticationService } from 'src/app/@data/services';

@Component({
  selector: 'serv-talento-implementacion',
  templateUrl: './implementacion.component.html',
  styleUrls: ['./implementacion.component.scss']
})
 
export class ImplementacionComponent implements OnInit {
  entidad: string = '';
  searchMode = false;
  const = Const;
  tableColumns: TableColumn[];
  ImplementacionService: any;
  private currentUserSubject: BehaviorSubject<User>;
  public currentUser: Observable<User>;

  constructor(
    private fb: FormBuilder,
    private dialog: MatDialog,
    private maeParametroRepository: MaestraParametroRepository,
    private authenticationService: AuthenticationService,
    private parameterRepository: ParameterRepository,
    private route: ActivatedRoute,
    private router: Router,
    private activatedRoute: ActivatedRoute,
    private toastService: ToastService,
    private cicloRepository: CicloRepository,
    private implementacionRepository: ImplementacionRepository,
  ) { }

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
  data: any[] = [];
  params: null;
  fieldsize = 'medium';

  ngOnInit(): void {
    this.anioFilter = this.fecha.getFullYear();
    this.loadCombox();
    this.initForm();
    this.initializeColumns();
  }
  initializeColumns() {
    this.tableColumns = [
      { name: 'Ciclo', dataKey: 'anioCiclo', position: 'left', isSortable: true },
      { name: 'Nivel de gobierno', dataKey: 'nivelGobierno', position: 'left', isSortable: true },
      { name: 'Sector', dataKey: 'sector', position: 'left', isSortable: true },
      { name: 'Tipo de entidad', dataKey: 'tipoEntidad', position: 'left', isSortable: true },
      { name: 'Entidad', dataKey: 'entidad', position: 'left', isSortable: true },
      { name: 'Gestor GDR', dataKey: 'contGestor', position: 'left', isSortable: true },
      { name: 'Estado de implementación', dataKey: 'colorEstado', innerHtml: true, isSortable: true },
    ];
  }

  loadCombox() { 
    const getNivel = this.parameterRepository.getNiveles();
    const getSector = this.parameterRepository.getSectores();
    const getCicloFilter = this.cicloRepository.getCiclosFilterFormalizados();
    const getTipoFilter = this.parameterRepository.getTipoEntidad();
    const getListEntidad = this.implementacionRepository.getListEntidades();
    const getListImpl = this.implementacionRepository.getListImplementacion(this.anioFilter);
    forkJoin([getNivel, getSector, getCicloFilter, getTipoFilter, getListEntidad, getListImpl]).subscribe(
      (results) => {
        this.nivel = results[0];
        this.sector = results[1];
        this.cicloFilter = results[2];
        this.tipoEntidad = results[3];
        this.listaEntidades = results[4];
        this.lstImplementacion = results[5];
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  initForm() {
    this.frm = this.fb.group({
      ciclo: [this.anioFilter, [Validators.required]],
      nivel: '',
      sector: '',
      tipoEntidad: '',
      entidad: '',
    });
  }

  get f() {
    return this.frm.controls;
  }

  files: File[] = [];

  clear() {
    this.anioFilter = this.fecha.getFullYear();
    this.loadCombox();
    this.initForm();
    this.initializeColumns();
  }

  search() {
    let ciclo, nivel, sector, tipoEnt, entidad = null;
    if ( this.frm.value.ciclo ) { ciclo = this.frm.value.ciclo; }
    if ( this.frm.value.nivel ) { nivel = this.frm.value.nivel; }
    if ( this.frm.value.sector ) { sector = this.frm.value.sector; }
    if ( this.frm.value.tipoEntidad ) { tipoEnt = this.frm.value.tipoEntidad; }
    if ( this.frm.value.entidad ) {
      if ( this.frm.value.entidad.siglaDescripcion ) {
        let val  = this.frm.value.entidad.siglaDescripcion.split(' - ');
        let entidadBusca = this.listaEntidades.find(item => item.descripcionEntidad === val[1] );
        entidad = entidadBusca.descripcionEntidad;
      } else {
        entidad = this.frm.value.entidad;
      }
    }
    this.implementacionRepository.getListImplementacion(
        ciclo, nivel, sector, tipoEnt, entidad)
        .subscribe(result => {
          this.lstImplementacion = result;
      });
  }

  onChangeEntidad($event: any) {
    let valor = $event.target.value.trim().toLowerCase();
    this.listaEntidadesFiltradas = this.listaEntidades.filter(x => x.siglaDescripcion.toLowerCase().includes(valor));
  }

  onSelectionChangeEntidad(idEntidad: number) {
    let seleccionado = this.listaEntidades.find(x => x.id === idEntidad);
    if (seleccionado) {
      this.frm.get('entidad').setValue(seleccionado.descripcionEntidad);
    }
  }
 
  asignarUsuarioGdr(row: any) {
    this.router.navigateByUrl('pages/gestor-gdr/' + row.entidadId);
  }

  openMonitor($event: any) {
 
    sessionStorage.setItem('originalEntidad', sessionStorage.getItem('entidad'));
    sessionStorage.setItem('cicloIdMonitoreo', $event.cicloId);
 
    this.authenticationService
    .cambiarEntidad($event.entidadId)
    .subscribe(
      (res) => {
        this.redirectMonitoreo();
      },
      (err) => {
        this.authenticationService.clearUser();
        this.toastService.showToast(err.message, 'danger');
      }
    );

  }

  redirectMonitoreo() {

this.implementacionRepository.getOptionsMenuTemp()
      .subscribe(result => {
        this.router.navigate(['pages/home'] );
      }, error => {
        this.toastService.showToast("Error al abrir vista de monitor", 'danger');
      });
  }

}
