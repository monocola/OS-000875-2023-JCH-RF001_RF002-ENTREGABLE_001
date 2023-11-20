import { Component, OnInit } from '@angular/core';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';
import { forkJoin } from 'rxjs';

@Component({
  selector: 'serv-talento-maestra-orh',
  templateUrl: './maestra-orh.component.html',
  styleUrls: ['./maestra-orh.component.scss']
})
export class MaestraOrhComponent implements OnInit {

  firstMessageTooltip = 'Ahora debes configurar tus tablas maestras ENTIDAD';
  secondMessageTooltip = 'Ahora debes configurar tus tablas maestras ENTIDAD';

  arrayMaestras = [];

  data = [];
  changeTrigger0 = false;
  changeTrigger1 = false;

  entidadChanged = false;
  servirChanged = false;

  constructor(
    private maestraService: MaestraRepository
  ) { }

  ngOnInit(): void {
    this.loadCombox();
  }

  loadCombox() {
    const getCabeceras = this.maestraService.getMaestraList();
    forkJoin([getCabeceras]).subscribe(
      results => {
        this.arrayMaestras = results[0];
        this.arrayMaestras.push({
          descripcion: 'Todas',
          maeCabeceraId: '-'
        });
      }
    );
  }

  handleEmitServir() {
    this.servirChanged = true;
  }

  handleEmitEntidad() {
    this.entidadChanged = true;
  }

  changeTab(e) {
    if (e.index === 0 && this.entidadChanged) {
      this.changeTrigger0 = !this.changeTrigger0;
      this.entidadChanged = false;
    }

    if (e.index === 1 && this.servirChanged) {
      this.changeTrigger1 = !this.changeTrigger1;
      this.servirChanged = false;
    }

  }

}
