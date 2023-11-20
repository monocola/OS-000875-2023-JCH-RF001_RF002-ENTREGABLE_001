import { Component, Input, OnInit } from '@angular/core';

@Component({
  selector: 'serv-talento-cabecera',
  templateUrl: './cabecera-examen.component.html',
  styleUrls: ['./cabecera-examen.component.scss'],
})
export class CabeceraExamenComponent implements OnInit {
  @Input() cabecera: string;
  @Input() examenId: number;
  @Input() convocatoria: string;
  @Input() regimen: string;
  @Input() perfil: string;

  constructor() {}

  ngOnInit(): void {}
}
