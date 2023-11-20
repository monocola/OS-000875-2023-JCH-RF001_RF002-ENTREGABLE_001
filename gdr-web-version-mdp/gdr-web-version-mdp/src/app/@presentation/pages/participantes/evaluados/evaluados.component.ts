import { Component, OnInit } from '@angular/core';
import { IParticipanteEvaluador } from 'src/app/@data/model/participante';
import { ServidoresRepository } from '../../../../@domain/repository/servidores.repository';
import { ActivatedRoute, Router } from '@angular/router';
import { PageChangedEvent } from 'ngx-bootstrap/pagination';
import { MatDialog } from '@angular/material/dialog';
import { ToastService } from '../../../@common-components/toast';
import { AsignarMandoMedioComponent } from '../asignar-mando-medio/asignar-mando-medio.component';
import { ModalEliminarEvaluadoComponent } from '../modal-eliminar-evaluado/modal-eliminar.component';
import { AgregarEvaluadosComponent } from '../agregar-evaluados/agregar-evaluados.component';
import { Status } from '../../../../@data/model/reponse-request';
import { FormBuilder, FormGroup } from '@angular/forms';
import { MaestraParametroRepository } from 'src/app/@domain/repository/maestra-parametro.repository';
import { UnidadOrganicaRepository } from 'src/app/@domain/repository/unidad-organica.repository';
import { MaestraParametro } from 'src/app/@data/model/maestra-parametro';
import { UnidadOrganicaCombo } from 'src/app/@data/model/unidadOrganicaCombo';
import { forkJoin } from 'rxjs';
import { AgregarEvaluadosMmComponent } from '../agregar-evaluados-mm/agregar-evaluados-mm.component';

@Component({
  selector: 'serv-talento-evaluados',
  templateUrl: './evaluados.component.html',
  styleUrls: ['./evaluados.component.scss'],
})
export class EvaluadosComponent implements OnInit {
  evaluador: IParticipanteEvaluador = {};
  mandoMedio: IParticipanteEvaluador = {};
  listaMandosMedio: IParticipanteEvaluador[] = [];
  listaMandosMedioPage: IParticipanteEvaluador[] = [];
  listaEvaluados: IParticipanteEvaluador[] = [];
  listaEvaluadoresTemp: IParticipanteEvaluador[] = [];
  listaEvaluadosPage: IParticipanteEvaluador[] = [];
  tabMandoMedioVisible = false;
  btnAddMandoMedioVisible = true;
  cantidadConEvaluador: number = 0;
  cantidadSinEvaluador: number = 0;
  ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
  cicloDefaultDesc;
  cicloDefault;
  frm: FormGroup = null;
  holderText = 'Buscar';
  segmentos: MaestraParametro[] = [];
  legend: MaestraParametro[] = [];
  unidadOrganicaCbo: UnidadOrganicaCombo[];
  idSegmento: number = 0;
  idArea: number = 0;
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
    private servidoresRepository: ServidoresRepository,
    private route: ActivatedRoute,
    public dialog: MatDialog,
    private toastService: ToastService,
    private router: Router,
    private fb: FormBuilder,
    private maeParametroRepository: MaestraParametroRepository,
    private UnidadOrganicaRepository: UnidadOrganicaRepository
  ) {
    this.evaluador = JSON.parse(sessionStorage.getItem('selected_evaluador'));
    route.queryParamMap.subscribe((q) => {});
  }

  ngOnInit(): void {
    this.initForm();
    this.loadCombox();
    this.setCiclo();
    if (this.ciclo?.cicloId) {
      this.updateList();
    } else {
      this.toastService.showToast(
        'No se tiene seleccionado ningún ciclo',
        'danger'
      );
    }
  }

  initForm() {
    this.frm = this.fb.group({
      filterP: '',
      area: '',
      segmento: '',
    });
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

        console.log("RESULTS", this.legend)
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  setCiclo() {
    if (this.ciclo.length !== 0 && this.ciclo) {
      this.cicloDefaultDesc = ' - ' + this.ciclo.anio;
      this.cicloDefault = this.ciclo.cronogramaId;
    } else {
      this.cicloDefaultDesc = '';
      this.cicloDefault = 0;
      this.toastService.showToast('No se encuentra ciclo configurado', 'danger');
    }
  }

  listarEvaluados(evaluador: IParticipanteEvaluador) {
    this.servidoresRepository.listarEvaluadosEntidadSub(evaluador).subscribe((res) => {
      this.listaEvaluados = res;
      this.listaEvaluadoresTemp = res;
      this.cantidadConEvaluador = this.listaEvaluados.length;

      this.pageChanged({ page: 1, itemsPerPage: 9 });
    });

    this.servidoresRepository.listarEvaluadosSinEvaluador(evaluador)
      .subscribe(res => {
        this.cantidadSinEvaluador = res.length;
      });
      
  }

  listarEvaluadosPage(evaluador: IParticipanteEvaluador) {
    this.servidoresRepository.listarEvaluadosEntidadSub(evaluador).subscribe((res) => {
      this.listaEvaluados = res;
      this.listaEvaluadoresTemp = res;
      this.cantidadConEvaluador = this.listaEvaluados.length;

      this.pageChanged({ page: 1, itemsPerPage: 9 });
    });

    this.servidoresRepository.listarSinEvaluador(evaluador)
      .subscribe(res => {
        this.cantidadSinEvaluador = 0;
        this.cantidadConEvaluador = this.listaEvaluados.length;
      });
      
  }

  updateList() {
    this.listarEvaluados(this.evaluador);

    this.servidoresRepository
      .listarMandosMedio(this.evaluador)
      .subscribe((res) => {
        this.listaMandosMedio = res;
        this.pageChangedMandoMedio(1);
      });
  }

  pageChanged($event: PageChangedEvent = { page: 1, itemsPerPage: 9 }) {
    this.listaEvaluadosPage = this.listaEvaluados.slice(
      ($event.page - 1) * $event.itemsPerPage,
      $event.page * $event.itemsPerPage
    );
    this.aparecerInactivo = false;
    this.aparecerPendiente = false;
    this.aparecerSuscrito = false;
    this.aparecerObservado = false;
    this.aparecerSinRegistro = false;

    this.listaEvaluadosPage.forEach( item => {
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
   
      switch (this.evaluador.indicadorMeta) {
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
  
    console.log("DATAAA:",  this.listaEvaluadosPage)
    console.log("Inactivo:",  this.aparecerInactivo)
    console.log("Sin registro:",  this.aparecerSinRegistro)
    console.log("Pendiente:",  this.aparecerPendiente)
    console.log("Observado:",  this.aparecerObservado)
    console.log("Suscrito:",  this.aparecerSuscrito) 
  }

  pageChangedMandoMedio(page: number) {
    this.listaMandosMedioPage = this.listaMandosMedio.slice(page - 1, page);
    if (this.tabMandoMedioVisible) {
      if (this.listaMandosMedioPage.length > 0) {
        let evaluadorMM = { ...this.listaMandosMedioPage[0]};
        this.listarEvaluadosPage(evaluadorMM);
      }
    }
  }

  eliminar(item: IParticipanteEvaluador, titulo: string) {
    if (item.cantidadEvaluados>0) {
      this.toastService.showToast(
        'Mando Medio tiene evaluados asignados',
        'danger'
      );
      return;
    }
    const evaluador = this.tabMandoMedioVisible
      ? this.listaMandosMedioPage[0]
      : this.evaluador;
    this.dialog
      .open(ModalEliminarEvaluadoComponent, {
        data: {
          titulo: titulo,
        },
      })
      .afterClosed()
      .subscribe((x) => {
        if (x) {
          this.servidoresRepository.eliminarEvaluado(item).subscribe(
            ({ success, error }) => {
              if (success) {
                this.listarEvaluados(evaluador);
                this.toastService.showToast(error.messages[0], 'success');
              }
              this.updateList()
            },
            (err) => this.toastService.showToast(err, 'danger')
          );
        }
      });
  }

  agregarMandoMedio() {
    this.dialog
      .open(AsignarMandoMedioComponent, {
        data: {
          evaluador: this.evaluador,
        },
        disableClose: true,
      })
      .afterClosed()
      .subscribe((status: Status) => {
        if (status?.success) {
          this.listarEvaluados(this.evaluador);
          this.toastService.showToast(status?.error.messages[0], 'success');
        }
        this.updateList()
      });
  }

  agregaEvaluados() {
    const evaluador = this.tabMandoMedioVisible
      ? this.listaMandosMedioPage[0]
      : this.evaluador;
    this.dialog
      .open(AgregarEvaluadosComponent, {
        data: {
          evaluador,
        },
        disableClose: true,
      })
      .afterClosed()
      .subscribe((status: Status) => {
        if (status?.success) {
          this.listarEvaluados({ ...evaluador, personaEvaluadorId: evaluador.personaId });
          this.toastService.showToast(status?.error.messages[0], 'success');
        }
        this.updateList()
      });
  }

  agregaEvaluadosMM() {
    const evaluador = this.tabMandoMedioVisible
      ? this.listaMandosMedioPage[0]
      : this.evaluador;
    this.dialog
      .open(AgregarEvaluadosMmComponent, {
        data: {
          evaluador,
        },
        disableClose: true,
      })
      .afterClosed()
      .subscribe((status: Status) => {
        if (status?.success) {
          this.listarEvaluadosPage({ ...evaluador});
          this.toastService.showToast(status?.error.messages[0], 'success');
        }
      });
  }

  onTabChanged(index: number) {
    this.tabMandoMedioVisible = index > 0;
    this.btnAddMandoMedioVisible = index === 0;
    if (index === 0) {
      this.listarEvaluados(this.evaluador);
    } else if (index === 1) {
      if (this.listaMandosMedioPage.length > 0) {
        this.listarEvaluadosPage(this.listaMandosMedioPage[0]);
      } else {
        this.listaEvaluados = [];
        this.listaEvaluadosPage = [];
        this.listaEvaluadoresTemp = [];
        this.cantidadConEvaluador = 0;
      } 
    }
    
  }

  onClickMetas(participante: IParticipanteEvaluador) {
    sessionStorage.setItem('selected_participante', JSON.stringify(participante));
    this.router.navigate(['/pages/participantes/evaluados/metas']);
  }

  changeArea(idArea: number) {
    this.idArea = idArea;

    if (this.idArea > 0 && this.idSegmento <= 0) {
      this.listaEvaluados = this.listaEvaluadoresTemp;
      this.listaEvaluados = this.listaEvaluados.filter(function (evalua) {
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

    if (this.idArea <= 0 && this.idSegmento > 0) {
      this.listaEvaluados = this.listaEvaluadoresTemp;
      this.listaEvaluados = this.listaEvaluados.filter(function (evalua) {
        return evalua.segmentoId === idSegmento;
      });
      this.pageChanged();
    }

    if (this.idArea > 0 && this.idSegmento > 0) {
      this.filtrarAreaSegmento(this.idArea, this.idSegmento);
    }
  }

  filtrarAreaSegmento(area, segmento) {
    this.listaEvaluados = this.listaEvaluadoresTemp;
    this.listaEvaluados = this.listaEvaluados.filter(function (evalua) {
      return evalua.segmentoId === segmento && evalua.uoId === area;
    });
    this.pageChanged();
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
    this.listaEvaluados = this.listaEvaluadoresTemp;
    this.listaEvaluados = this.listaEvaluados.filter(function (evalua) {
      return expresion.test(evalua.apellidosNombres);
    });
    this.pageChanged();
  }
}
