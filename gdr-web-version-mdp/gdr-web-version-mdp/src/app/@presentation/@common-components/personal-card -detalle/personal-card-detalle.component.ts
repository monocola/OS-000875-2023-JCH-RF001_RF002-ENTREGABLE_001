import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { Const } from 'src/app/@data/services/const';
import { Utils } from 'src/app/utils/utils';

@Component({
  selector: 'personal-card-detalle',
  templateUrl: './personal-card-detalle.component.html',
  styleUrls: ['./personal-card-detalle.component.scss'],
})
export class PersonalCardDetalleComponent implements OnInit {
  @Input() informacion: any = {};
  @Input() mostrarEtiquetaUsuario: boolean = false;
  @Input() etiquetaUsuario: string = 'Metas';
  @Input() urlImagen: string = '';
  @Input() nombre: string = '';
  @Input() segmentoCorto: string = '';

  @Input() cargo: string = '';
  @Input() nivel: string = '';
  @Input() mostrarBotonLink: boolean = false;
  @Input() etiquetaLink: string = 'Ir a Metas y Compromisos';
  @Input() cantidadParticipantes: number = 0;
  @Input() mostrarParticipantes: boolean = false;
  @Input() mostrarCerrar: boolean = false;
  @Input() clickable: boolean = false;
  @Input() metas: number;
  @Input() compromisos: number;
  @Output() cerrarClick: EventEmitter<any> = new EventEmitter();
  @Output() botonLinkClick: EventEmitter<any> = new EventEmitter();
  @Output() cardClick: EventEmitter<any> = new EventEmitter();
  @Output() metasClick: EventEmitter<any> = new EventEmitter();
  mostrarTexto: boolean;

  urlServidorFiles: string;

  constructor() {}

  ngOnInit(): void {
    this.urlServidorFiles = Const.API_FILE_SERVER + this.urlImagen;
  }

  onCerrarClick(): void {
    this.cerrarClick.emit(this.informacion);
  }

  onBotonLinkClick(): void {
    this.botonLinkClick.emit(this.informacion);
  }

  onCardClick(): void {
    this.cardClick.emit(this.informacion);
  }

  onMetasClick(): void {
    this.metasClick.emit(this.informacion);
  }

  get colorMeta() {
    switch (this.metas) {
      case 0:
        return 'gray';
      case 1:
        return 'danger';
      case 2:
        return 'warning';
      case 3:
        return 'info';
      case 4:
        return 'success';
    }
  }

  cortarTexto(texto) {
    if (texto.length > 30) {
      return Utils.cortarCaracteres(
        texto,
        Const.CORTAR_NUM_CARACTERES_PARTICIPANTES
      );
    } else {
      return texto;
    }
  }
}
