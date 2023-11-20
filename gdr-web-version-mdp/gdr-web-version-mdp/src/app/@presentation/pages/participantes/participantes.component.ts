import { Component, HostListener, OnInit, SystemJsNgModuleLoader } from '@angular/core';
import { ToastService } from '../../@common-components/toast';
import { MatDialog } from '@angular/material/dialog';
import { Router } from '@angular/router';
import { ServidoresRepository } from '../../../@domain/repository/servidores.repository';
import { IParticipanteEvaluador } from 'src/app/@data/model/participante';
import { PageChangedEvent } from 'ngx-bootstrap/pagination';
import { FormBuilder, FormGroup } from '@angular/forms';
import { MaestraParametro } from 'src/app/@data/model/maestra-parametro';
import { forkJoin } from 'rxjs';
import { UnidadOrganicaRepository } from 'src/app/@domain/repository/unidad-organica.repository';
import { MaestraParametroRepository } from 'src/app/@domain/repository/maestra-parametro.repository';
import { UnidadOrganicaCombo } from 'src/app/@data/model/unidadOrganicaCombo';

@Component({
  selector: 'serv-talento-participantes',
  templateUrl: './participantes.component.html',
  styleUrls: ['./participantes.component.scss'],
})
export class ParticipantesComponent implements OnInit {
  listaEvaluadores: IParticipanteEvaluador[] = [];
  listaEvaluadoresTemp: IParticipanteEvaluador[] = [];
  countListaEvaluadoresConEvaluador: IParticipanteEvaluador[] = [];
  countListaEvaluadoresSinEvaluador: IParticipanteEvaluador[] = [];
  listaEvaluadoresPage: IParticipanteEvaluador[] = [];
  evaluador: IParticipanteEvaluador = {};
  holderText = 'Buscar';
  ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
  cicloDefaultDesc;
  cicloDefault;
  frm: FormGroup = null;
  segmentos: MaestraParametro[] = [];
  unidadOrganicaCbo: UnidadOrganicaCombo[];
  idSegmento: number = 0;
  idArea: number = 0;
  legend: MaestraParametro[] = [];
  suscrito = '';
  pendiente = '';
  sinregistro = '';
  observado = '';
  inactivo = '';
  aparecerSuscrito : boolean = false;
  aparecerPendiente : boolean = false;
  aparecerSinRegistro : boolean = false;
  aparecerObservado : boolean = false;
  aparecerInactivo : boolean = false;

  constructor(
    private toastService: ToastService,
    public dialog: MatDialog,
    private servidoresRepository: ServidoresRepository,
    private router: Router,
    private fb: FormBuilder,
    private maeParametroRepository: MaestraParametroRepository,
    private UnidadOrganicaRepository: UnidadOrganicaRepository
  ) {}
  cantidadConEvaluador = 0;
  cantidadSinEvaluador = 0;
  filterListaEvaluadores: IParticipanteEvaluador[] = [];
  filterP = '';

  ngOnInit(): void {
    this.initForm();
    this.setCiclo();
    this.loadCombox();
    if (this.ciclo?.cicloId) {
      if (this.ciclo?.estadoCicloId) {
        this.listarEvaluadores();
      } else {
        this.toastService.showToast(
          'El ciclo seleccionado no se encuentra vigente',
          'danger'
        );
      }
    } else {
      this.listaEvaluadores = [];
      this.listaEvaluadoresTemp = [];
      this.toastService.showToast(
        'No se tiene seleccionado ningún ciclo',
        'danger'
      );
    }
  }

  loadCombox() {
    this.segmentos = [];
    this.legend = [];

    const getSegmento = this.maeParametroRepository.getMaestraParametro(
      'SEGMENTO_GDR'
    );
    const getLegend = this.maeParametroRepository.getMaestraParametro(
      'SEMAFORO_LEGEND'
    );
    const getUndOrganicaCbo = this.UnidadOrganicaRepository.getUnidadOrganicaCbo();

    forkJoin([getSegmento, getUndOrganicaCbo, getLegend]).subscribe(
      (results) => {
        this.segmentos = results[0];
        this.unidadOrganicaCbo = results[1];
        this.legend = results[2] 
        this.legend =this.legend.sort((a, b) => a.parametroId- b.parametroId);
        this.suscrito = this.legend[0].valorTexto
        this.pendiente = this.legend[1].valorTexto
        this.sinregistro = this.legend[2].valorTexto
        this.observado = this.legend[3].valorTexto
        this.inactivo = this.legend[4].valorTexto
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  initForm() {
    this.frm = this.fb.group({
      filterP: '',
      area: '',
      segmento: '',
    });
  }

  get f() {
    return this.frm.controls;
  }

  listarSinEvaluadores() {
    this.servidoresRepository
      .listarEvaluadosSinEvaluadorEntidad(this.evaluador)
      .subscribe((res) => {
        this.cantidadSinEvaluador = res.length;
      });
  }

  busquedaSensitiva($event) {
    this.filtrar();
  }

  onKeydown(event) {
    console.log(event);
    this.filtrar();
  }

  filtrar() {
    let busqueda = this.frm.value.filterP;
    let expresion = new RegExp(`${busqueda}.*`, 'i');
    this.listaEvaluadores = this.listaEvaluadoresTemp;
    this.listaEvaluadores = this.listaEvaluadores.filter(function (evalua) {
      return expresion.test(evalua.apellidosNombres);
    });

    this.pageChanged();
  }

  listarEvaluadores() {
    this.servidoresRepository.listarEvaluadoresentidad().subscribe(
      (res) => {
        this.listaEvaluadores = res;
        this.listaEvaluadoresTemp = res;
        this.countListaEvaluadoresConEvaluador = this.listaEvaluadoresTemp.filter(
          function (evalua) {
            return evalua.flagEvaluador === true;
          }
        );
        this.countListaEvaluadoresSinEvaluador = this.listaEvaluadoresTemp.filter(
          function (evalua) {
            return evalua.flagEvaluador === false;
          }
        );

        this.cantidadSinEvaluador = this.countListaEvaluadoresSinEvaluador.length;
        this.cantidadConEvaluador = this.countListaEvaluadoresConEvaluador.length;
        console.log("PARTICIPANTEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEES"+ this.cantidadConEvaluador + "||" + this.cantidadSinEvaluador)
        if (this.listaEvaluadores.length === 0) {
          this.toastService.showToast(
            'No se encontraron resultados',
            'primary'
          );
        }
        this.pageChanged();
      },
      (err) => {
        this.toastService.showToast(err.message, 'danger');
      }
    );
    
  }

  pageChanged($event: PageChangedEvent = { page: 1, itemsPerPage: 9 }) {
    this.listaEvaluadoresPage = this.listaEvaluadores.slice(
      ($event.page - 1) * $event.itemsPerPage,
      $event.page * $event.itemsPerPage
    );
    this.aparecerInactivo = false;
    this.aparecerPendiente = false;
    this.aparecerSuscrito = false;
    this.aparecerObservado = false;
    this.aparecerSinRegistro = false;

    this.listaEvaluadoresPage.forEach( item => {
      switch (item.indicadorMeta) {
        case 0:
          this.aparecerInactivo = true;
          break;
        case 1:
          this.aparecerSinRegistro = true; 
          break;
        case 2:
          this.aparecerPendiente = true;
          break;
        case 3:
          this.aparecerObservado = true;
          break;
        case 4:
          this.aparecerSuscrito = true;
          break;
      }
    });
    console.log("DATAAA:",  this.listaEvaluadoresPage)
    console.log("Inactivo:",  this.aparecerInactivo)
    console.log("Sin registro:",  this.aparecerSinRegistro)
    console.log("Pendiente:",  this.aparecerPendiente)
    console.log("Observado:",  this.aparecerObservado)
    console.log("Suscrito:",  this.aparecerSuscrito)

  }

  onClickParticipante(participante: IParticipanteEvaluador) {
    sessionStorage.setItem('selected_evaluador', JSON.stringify(participante));
    this.router.navigate(['/pages/participantes/evaluados']);
  }

  onClickMetas(participante: IParticipanteEvaluador) {
    participante.vieneDeParticipantes = true;
    sessionStorage.setItem('selected_participante', JSON.stringify(participante));
    participante.vieneDeParticipantes = null;
    this.router.navigate(['/pages/participantes/evaluados/metas']);
  }

  setCiclo() {
    if (this.ciclo.length !== 0 && this.ciclo) {
      //this.cicloDefaultDesc = ' - ' + this.ciclo.anio;
      this.cicloDefaultDesc = this.ciclo.anio;
      this.cicloDefault = this.ciclo.cronogramaId;
    } else {
      this.cicloDefaultDesc = '';
      this.cicloDefault = 0;
      this.toastService.showToast(
        'No se encuentra ciclo configurado',
        'danger'
      );
    }
  }

  applyFilter(event) {
    console.info(event);
    const filterValue = (event.target as HTMLInputElement).value.toLowerCase();
    console.info(filterValue);

    // this.tableDataSource.filter = filterValue.trim().toLowerCase();
  }

  changeArea(idArea: number) {
    this.idArea = idArea;

    console.log('areaa:', this.idArea);

    if (this.idArea === -1 && this.idArea === -1) {
      this.listaEvaluadores = this.listaEvaluadoresTemp;
      this.pageChanged();
      return;
    }

    if (this.idArea > 0 && this.idSegmento <= 0) {
      this.listaEvaluadores = this.listaEvaluadoresTemp;
      this.listaEvaluadores = this.listaEvaluadores.filter(function (evalua) {
        return evalua.uoId === idArea;
      });
      this.pageChanged();
    }

    if (this.idArea > 0 && this.idSegmento > 0) {
      this.filtrarAreaSegmento(this.idArea, this.idSegmento);
    }
  }

  changeSegmento(idSegmento: number) {
    this.idSegmento = idSegmento;

    if (this.idSegmento === -1 && this.idArea === -1) {
      this.listaEvaluadores = this.listaEvaluadoresTemp;
      this.pageChanged();
      return;
    }

    if (this.idArea <= 0 && this.idSegmento > 0) {
      this.listaEvaluadores = this.listaEvaluadoresTemp;
      this.listaEvaluadores = this.listaEvaluadores.filter(function (evalua) {
        return evalua.segmentoId === idSegmento;
      });
      this.pageChanged();
    }

    if (this.idArea > 0 && this.idSegmento > 0) {
      this.filtrarAreaSegmento(this.idArea, this.idSegmento);
    }
  }

  filtrarAreaSegmento(area, segmento) {
    this.listaEvaluadores = this.listaEvaluadoresTemp;
    this.listaEvaluadores = this.listaEvaluadores.filter(function (evalua) {
      return evalua.segmentoId === segmento && evalua.uoId === area;
    });
    this.pageChanged();
  }
}
