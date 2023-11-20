import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { AngularResizedEventModule } from 'angular-resize-event';
import { BottomDivComponent } from './bottom-div/bottom-div.component';
import { RouterModule } from '@angular/router';
import { NbButtonModule, NbIconModule } from '@nebular/theme';
import { ModalCreacionBonificacionComponent } from './modal-creacion-bonificacion/modal-creacion-bonificacion.component';
import { BonificacionInstanceComponent } from './bonificacion-instance/bonificacion-instance.component';
import { CommonComponentsModule } from 'src/app/@presentation/@common-components/common-components.module';
import { ReactiveFormsModule } from '@angular/forms';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';
import { QuillModule } from 'ngx-quill';

@NgModule({
  declarations: [
    BottomDivComponent,
    ModalCreacionBonificacionComponent,
    BonificacionInstanceComponent,
  ],
  imports: [
    CommonModule,
    AngularResizedEventModule,
    RouterModule,
    NbButtonModule,
    CommonComponentsModule,
    NbIconModule,
    ReactiveFormsModule,
    MatIconModule,
    MatButtonModule,
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
  exports: [BottomDivComponent, BonificacionInstanceComponent],
})
export class ComponentsBasePlantillasModule {}
