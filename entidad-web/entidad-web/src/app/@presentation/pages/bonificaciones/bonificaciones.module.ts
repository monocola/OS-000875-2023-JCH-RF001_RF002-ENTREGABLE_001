import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { BonificacionesRoutingModule } from './bonificaciones-routing.module';
import { BonificacionesComponent } from './bonificaciones.component';
import { MatDividerModule } from '@angular/material/divider';
import { NbButtonModule, NbIconModule } from '@nebular/theme';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { CreacionBonificacionComponent } from './creacion-bonificacion/creacion-bonificacion.component';
import { GestionBonificacionComponent } from './gestion-bonificacion/gestion-bonificacion.component';
import { RouterModule } from '@angular/router';
import { AngularResizedEventModule } from 'angular-resize-event';
import { MatButtonModule } from '@angular/material/button';
import { QuillModule } from 'ngx-quill';
import { ModalNuevoDetalleBonificacionComponent } from './modal-nuevo-detalle-bonificacion/modal-nuevo-detalle-bonificacion.component';
import { ComponentsBasePlantillasModule } from '../bases-plantillas/components/components-bases.plantillas.module';

@NgModule({
  declarations: [
    BonificacionesComponent,
    CreacionBonificacionComponent,
    GestionBonificacionComponent,
    ModalNuevoDetalleBonificacionComponent,
  ],
  imports: [
    CommonModule,
    CommonComponentsModule,
    BonificacionesRoutingModule,
    MatDividerModule,
    NbButtonModule,
    ReactiveFormsModule,
    FormsModule,
    RouterModule,
    NbIconModule,
    AngularResizedEventModule,
    NbButtonModule,
    MatButtonModule,
    ComponentsBasePlantillasModule,
    QuillModule.forRoot({
      modules: {
        syntax: false,
        toolbar: [
          ['bold', 'italic', 'underline'],
          [{ list: 'bullet' }],
          [{ header: [1, 2, 3, 4, 5, 6, false] }],
          [{ align: [] }],
        ],
      },
    }),
  ],
})
export class BonificacionesModule {}
