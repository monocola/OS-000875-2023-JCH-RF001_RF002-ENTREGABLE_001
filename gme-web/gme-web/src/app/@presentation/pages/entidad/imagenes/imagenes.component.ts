import { Component, Input, OnInit } from '@angular/core';

@Component({
  selector: 'gme-web-imagenes',
  templateUrl: './imagenes.component.html',
})
export class ImagenesComponent implements OnInit {
 
  @Input() imgEntidad;

  constructor(
  ) {}

  ngOnInit(): void {
    console.log("imagenes : ",this.imgEntidad)
  }

}


