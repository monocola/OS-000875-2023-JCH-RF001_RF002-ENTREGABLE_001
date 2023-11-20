import {
  AfterViewInit,
  Component,
  ElementRef,
  Input,
  OnChanges,
  OnInit,
  SimpleChanges,
  ViewChild,
} from '@angular/core';
import html2canvas from 'html2canvas';
import { saveAs } from 'file-saver';
import { Organo } from 'src/app/@data/model/organo';
import { NgxSpinnerService } from 'ngx-spinner';

import * as panzoom from 'panzoom';
import { Router } from '@angular/router';
import { OrganigramaRepository } from 'src/app/@domain/repository/organigrama.repository';
import { MaestraParametroRepository } from 'src/app/@domain/repository/maestra-parametro.repository';
import { MaestraParametro } from 'src/app/@data/model/maestra-parametro';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
declare var google: any;


@Component({
  selector: 'gme-web-graph',
  templateUrl: './graph.component.html',
  styleUrls: ['./graph.component.scss']
})
export class GraphComponent implements OnInit, OnChanges, AfterViewInit {
  @ViewChild('btnCompleto', { read: ElementRef }) btnCompleto: ElementRef;
  @Input() organos: Organo[] = [];
  @Input() unidadesOrganicas: any[] = [];

  @Input() organosFinded: any[] = [];
  @Input() organosFindedByPuesto: any[] = [];

  altasDirecciones: any[] = [];
  organosAndUnidades: any[] = [];
  index = 0;
  showFilters = false;
  showFull = false;
  idHardCodeParent = null;
  loadingGraph = false;
  downloadingGraph = false;
  parametros: MaestraParametro[];
  initialSettings = {
    maxZoom: 2,
    minZoom: 0.2,
    initialZoom: 1,
  };

  constructor(
    private spinner: NgxSpinnerService,
    private router: Router,
    private organigramaRepository: OrganigramaRepository,
    private maeParametroRepository: MaestraParametroRepository,
    private authenticationService: AuthenticationRepository,
    ) { }

  ngOnInit() {
    this.showFull = true;
    this.loadingGraph = true;
    this.maeParametroRepository.getMaestraParametro('TIPO_NATURALEZA')
    .subscribe(res => this.parametros = res );
  }
  
  ngAfterViewInit() {
    setTimeout(() => {
      this.btnCompleto.nativeElement.click();
    }, 500);
  }

  ngOnChanges(changes: SimpleChanges): void {
    if (changes.organos?.currentValue) {
      const organos = changes.organos.currentValue;
      const unidadesOrganicas = changes.unidadesOrganicas.currentValue;
      this.setInitialGraph(organos, unidadesOrganicas);
      this.showFullOrganigrama();
    } else {
      if (changes.organosFinded?.currentValue) {
        const arrayToRender = changes.organosFinded.currentValue;
        if (arrayToRender.length > 0) {
          this.setOrgChart(arrayToRender);
        } else {
          if (this.showFull) {
            // this.selectKindCharts(0);
            this.showFullOrganigrama();
          } else {
            this.setInitialGraph(this.organos, this.unidadesOrganicas);
          }
        }
      }
    }
  }

  setInitialGraph(organosToRender, UnidadesToRender) {
    this.altasDirecciones = [];
    this.organosAndUnidades = [];
    const organos: Organo[] = organosToRender.slice(0);
    const unidades: any[] = UnidadesToRender.slice(0);
    organos.forEach((o) => (o.tipo = 1));
    unidades.forEach((u) => (u.tipo = 2));
    let items = organos.filter(el => el.estado === 'ACTIVO');
    // unidades.forEach(el => {
    //   let rep = organos.find(org => org.descripcion === el.unidadOrganica && org.estado === 'ACTIVO');
    //   if (!rep) {
    //     items.push(el);
    //   }
    // });
    if (items.length > 0) {
      this.organosAndUnidades = items.slice(0);
      this.separarAltasDirecciones();
      this.setOrgChart(this.altasDirecciones[0]);
    } else {
      this.loadingGraph = false;
    }
  }

  clickFiltro() {
    this.showFilters = !this.showFilters;
  }

  initializeButtons() {
    const buttons = document.getElementsByClassName('button__org_chart');
    for (let index = 0; index < buttons.length; index++) {
      const id = Number(buttons[index].getAttribute('id'));
      const element = buttons[index];

      // NOSONAR
      element.addEventListener('click', () => {
        // NOSONAR
        const data = this.organosAndUnidades.filter(
          (item) => item.organigramaId === id
        )[0];
        this.organigramaRepository.setOrganoOrUnidadFromChart(data);
        this.router.navigateByUrl('pages/organigrama/configuracion');
      });
    }
  }

  separarAltasDirecciones() {
    this.organosAndUnidades.forEach((item) => {
      if (
        (!item.padreOrganigramaId && item.estadoRegistro === '1') ||
        item.padreOrganigramaId !== this.idHardCodeParent
      ) {
        // item.tipo = 0;
        if (item.padreOrganigramaId === this.idHardCodeParent) {
          item.padreOrganigramaId = '';
        }
        this.altasDirecciones.push([item]);
      }
    });
    this.altasDirecciones.forEach((altaDireccion, index) =>
      this.getHijosDeAltaDireccion(altaDireccion[0], index)
    );
  }

  getHijosDeAltaDireccion(organoOrUnidad, index) {
    const hijos = this.organosAndUnidades.filter((item) => {
      if (
        item.padreOrganigramaId === organoOrUnidad.organigramaId &&
        item.estadoRegistro === '1'
      ) {
        return item;
      }
    });
    hijos.forEach((hijo) => {
      this.altasDirecciones[index].push(hijo);
      // this.getHijosDeAltaDireccion(hijo, index);
    });
  }

  setOrgChart(dataToRender) {
    google.charts.load('current', { packages: ['orgchart'] });
    google.charts.setOnLoadCallback(() => {
      let data = new google.visualization.DataTable();
      data.addColumn('string', 'Name');
      data.addColumn('string', 'Manager');
      data.addColumn('string', 'ToolTip');

      const dataFormatted = this.formatDataToRender(dataToRender);
      data.addRows(dataFormatted);
      const chartContainer = document.getElementById('chart_div');
      let chart = new google.visualization.OrgChart(chartContainer);
      google.visualization.events.addListener(chart, 'ready', () => {
        panzoom.default(
          document.querySelector('#chart_div'),
          this.initialSettings
        );
        this.initializeButtons();
        this.loadingGraph = false;
      });

      chart.draw(data, { allowHtml: true });
    });
  }

  getIdParentMax(data: any[]) {
    let id = '';
    for (let index = 0; index < data.length; index++) {
      const element = data[index];
      if (element.idPadre === '') {
        id = element;
        break;
      }
    }
    return id;
  }

  formatDataToRender(data) {
    const datos = data?.map((d) => {
      let color = this.getColor(d.naturalezaOrgano);
      let  urlImg = d.urlFoto ? d.urlFoto : './assets/images/delver.jpg';
      const dataToRender = {
        v: d.organigramaId?.toString() || '',
        f: `
          ${d.tipo !== 3
            ? `
            <div class="body__graph__chart mat-elevation-z1">
              <div class="head_top " style="background:${color}"
              data-html2canvas-ignore="true"></div>
            
              <div class="card__header__org__chart">
                <div class="flex-v-center w-100">
                  <div class="title__card_org font-weight-bold" style="color:${color}">${d.descripcion || d.unidadOrganica
                  }</div>
                  <span class="flex-spacer"></span>
                </div>
              </div>
              <hr></hr>
              <div class="card__body__org__chart">
                <div style="position:relative">
                  <div class="item__card">
                    <img src="${urlImg}" class="img_org" alt="" height="50px" width="50px" style="border-radius: 50%; background-color: #92a8d1;" > 
                    <div style="padding-left: 10px!important;">
                      <div style="margin-bottom: 5px!important;">${this.setNombreCompleto(d)}</div> 
                      <span style="color:${color}">${d.puesto !== null ? d.puesto : ''}</span>
                    </div>
                  </div>
                </div>
              </div>
            </div>
            ` : `
            <div class="mat-elevation-z1">
              <div class="topBar__super_parent_graph">
              </div>
              <div class="super_parent__graph">
                ${d.descripcion}
              </div>
            </div>`
          }`,
      };
      const idPadre = d.padreOrganigramaId?.toString() || '';
      const tooltip = d.descripcion || d.unidadOrganica;
      return [dataToRender, idPadre, tooltip];
    });

    return datos;
  }

  setNombreCompleto(body) {
    if (body.nombres !== null && body.apellidoPaterno !== null) {
      return `${body.nombres} ${body.apellidoPaterno} ${body.apellidoMaterno || ''
        }`.trimRight();
    } else {
      return '';
    }
  }

  selectKindCharts(index) {
    let numero = index.target.value;
    this.showFull = false;
    this.index = Number(numero);
    this.setOrgChart(this.altasDirecciones[Number(numero)]);
  }

  centrarMapa() {
    const selector: any = document.querySelector('#chart_div');
    panzoom.default(selector, this.initialSettings);
  }

  downloadByPuestos() {
    window.scrollTo(0, 0);
    this.downloadingGraph = true;
    this.spinner.show();
    const divToPrint = document.getElementById('puestos__org');
    divToPrint.classList.toggle('overflowVisible');
    setTimeout(() => {
      html2canvas(divToPrint, {
        scale: 3,
        useCORS: true,
      }).then((canvas) => {
        const img = canvas.toDataURL('image/jpeg');
        saveAs(img, 'Organigrama.jpeg');
        this.downloadingGraph = false;
        divToPrint.classList.toggle('overflowVisible');
        this.spinner.hide();
      });
    }, 1000);
  }

  download() {
    this.downloadingGraph = true;
    this.spinner.show('spinnerGraph');
    const selector: HTMLElement = document.querySelector('#chart_div');
    const graph = panzoom.default(selector, this.initialSettings);
    graph.moveTo(0, 0);
    const divToPrint = document.getElementsByClassName(
      'google-visualization-orgchart-table'
    )[0] as HTMLElement;
    const widthGraph = document.getElementsByClassName(
      'google-visualization-orgchart-table'
    )[0].clientWidth;
    const heightGraph = document.getElementsByClassName(
      'google-visualization-orgchart-table'
    )[0].clientHeight;
    setTimeout(() => {
      html2canvas(divToPrint, {
        scale: 4,
        useCORS: true,
        width: widthGraph,
        height: heightGraph,
      }).then((canvas) => {
        const img = canvas.toDataURL('image/jpeg');
        saveAs(img, 'Organigrama.jpeg');
        this.downloadingGraph = false;
        this.spinner.hide('spinnerGraph');
      });
    }, 1000);
  }

  showFullOrganigrama() {
    this.showFull = true;
    const items = document.getElementsByClassName('item__navbar');
    for (let index = 0; index < items.length; index++) {
      const element = items[index];
      element.classList.remove('selected');
    }
    const dataToRender = this.organosAndUnidades.slice(0);
    const entidadId = JSON.parse(sessionStorage.getItem('persona'))
      .entidadId;
    const superParentItem = {
      organigramaId: this.idHardCodeParent,
      descripcion: '',
      entidadId: entidadId,
      padreOrganigramaId: '',
      tipo: 3,
    };
    this.setOrgChart(dataToRender);
  }

  getColor(organo: number) {
    let color = '';
    switch (organo) {
      case 82:
        color = '#27b3d6';
        break;
      case 83:
        color = '#3058dd';
        break;
      case 84:
        color = '#ee2323';
        break;
      case 85:
        color = '#eee023';
        break;
      case 86:
        color = '#3fc923';
        break;
      case 87:
        color = '#d65e26';
        break;
      case 88:
        color = '#82df76';
        break;
      case 89:
        color = '#443b3b';
        break;
      case 90:
        color = '#6e6e6e';
        break;
      case 91:
        color = '#638BF8';
        break;
      default:
        color = '#638bf8';
        break;
    }
    return color;
  }
}
