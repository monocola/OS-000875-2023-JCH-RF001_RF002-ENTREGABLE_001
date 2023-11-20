import {
  Component,
  Input,
  OnChanges,
  OnInit,
  SimpleChanges,
} from '@angular/core';
import html2canvas from 'html2canvas';
import { saveAs } from 'file-saver';
import { Organo } from 'src/app/@data/model/organo';
import { NgxSpinnerService } from 'ngx-spinner';

import * as panzoom from 'panzoom';
import { Router } from '@angular/router';
import { OrganigramaRepository } from 'src/app/@domain/repository/organigrama.repository';
declare var google: any;


@Component({
  selector: 'serv-talento-graph',
  templateUrl: './graph.component.html',
  styleUrls: ['./graph.component.scss'],
})
export class GraphComponent implements OnInit, OnChanges {
  @Input() organos: Organo[] = [];
  @Input() unidadesOrganicas: any[] = [];

  @Input() organosFinded: any[] = [];
  @Input() organosFindedByPuesto: any[] = [];

  altasDirecciones: any[] = [];
  organosAndUnidades: any[] = [];
  index = 0;
  showFilters = false;
  showFull = false;
  idHardCodeParent = '00000000';
  loadingGraph = false;
  downloadingGraph = false;
  initialSettings = {
    maxZoom: 2,
    minZoom: 0.2,
    initialZoom: 1,
  };

  constructor(
    private spinner: NgxSpinnerService,
    private router: Router,
    private organigramaRepository: OrganigramaRepository
  ) { }

  ngOnInit() {
    this.loadingGraph = true;
  }

  ngOnChanges(changes: SimpleChanges): void {
    if (changes.organos?.currentValue) {
      const organos = changes.organos.currentValue;
      const unidadesOrganicas = changes.unidadesOrganicas.currentValue;
      this.setInitialGraph(organos, unidadesOrganicas);
    } else {
      if (changes.organosFinded?.currentValue) {
        const arrayToRender = changes.organosFinded.currentValue;
        if (arrayToRender.length > 0) {
          this.setOrgChart(arrayToRender);
        } else {
          if (this.showFull) {
            this.selectKindCharts(0);
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
    const items = organos.concat(unidades);
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

      element.addEventListener('click', () => {
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
        item.padreOrganigramaId === this.idHardCodeParent
      ) {
        item.tipo = 0;
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
      this.getHijosDeAltaDireccion(hijo, index);
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
    const datos = data.map((d) => {
      const dataToRender = {
        v: d.organigramaId?.toString() || '',
        f: `
          ${d.tipo !== 3
            ? `
            <div class="body__graph__chart mat-elevation-z1">
            <div class="${d.tipo === 0
              ? 'blue0__top'
              : d.tipo === 1
                ? 'blue1__top'
                : 'blue2__top'
            }"
            data-html2canvas-ignore="true"></div>
            
            <div class="card__header__org__chart">
              <div class="flex-v-center w-100">
                <div class="title__card_org">${d.descripcion || d.unidadOrganica
            }</div>
                <span class="flex-spacer"></span>
                <button id="${d.organigramaId
            }" class="button__org_chart" data-html2canvas-ignore="true">
                  <i id="${d.organigramaId}" class="ent-web-edit"></i>
                </button>
              </div>
              <div class="subtitle__card_org">
                ${d.desNivel}
              </div>
            </div>

            <hr></hr>

            <div class="card__body__org__chart">
              <div class="card__body__puesto">${d.puesto}</div>
              <div style="position:relative">

                <div class="item__card"> 
                <i class="material-icons">
                  account_circle
                </i>
                  <span>${this.setNombreCompleto(d)}</span>
                </div>

                <div class="item__card"> 
                <i class="material-icons">
                  mail_outline
                </i>
                  <span>${d?.correo?.toLowerCase() || null}</span>
                </div>

                <div class="item__card"> 
                <i class="material-icons">
                  call
                </i>
                  <span>${d.telefono}</span>
                </div>
              </div>
            </div>
          </div>
            `
            : `
            <div class="mat-elevation-z1">
              <div class="topBar__super_parent_graph">
              </div>
              <div class="super_parent__graph">
              ${d.descripcion}
              </div>
            </div>
          `
          }
        `,
      };
      const idPadre = d.padreOrganigramaId?.toString() || '';
      const tooltip = d.descripcion || d.unidadOrganica;
      return [dataToRender, idPadre, tooltip];
    });

    return datos;
  }

  setNombreCompleto(body) {
    return `${body.nombres} ${body.apellidoPaterno} ${body.apellidoMaterno || ''
      }`.trimRight();
  }

  selectKindCharts(index: number) {
    this.showFull = false;
    const items = document.getElementsByClassName('item__navbar');
    for (let i = 0; i < items.length; i++) {
      const element = items[i];
      element.classList.remove('selected');
    }
    items[index].classList.add('selected');
    this.index = index;
    this.altasDirecciones.forEach((ad) => (ad[0].padreOrganigramaId = ''));
    this.setOrgChart(this.altasDirecciones[index]);
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
    const aux = this.organosAndUnidades.slice(0);
    const dataToRender = aux.filter((item) => item.estadoRegistro === '1');
    dataToRender.forEach((dato) => {
      if (!dato.padreOrganigramaId) {
        dato.padreOrganigramaId = this.idHardCodeParent;
      }
    });
    const entidadNombre = JSON.parse(sessionStorage.getItem('persona'))
      .entidadNombre;
    const superParentItem = {
      organigramaId: this.idHardCodeParent,
      descripcion: entidadNombre,
      padreOrganigramaId: '',
      tipo: 3,
    };
    dataToRender.push(superParentItem);
    this.setOrgChart(dataToRender);
  }
}
