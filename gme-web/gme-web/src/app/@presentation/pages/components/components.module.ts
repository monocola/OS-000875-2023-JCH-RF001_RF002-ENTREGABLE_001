import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { HeaderComponent } from './header/header.component';
import { SidenavComponent } from './sidenav/sidenav.component';
import { RouterModule } from '@angular/router';
import { NbEvaIconsModule } from '@nebular/eva-icons';
import {
  NbIconModule,
  NbLayoutModule,
  NbMenuModule,
  NbSidebarModule,
  NbToggleComponent,
  NbToggleModule,
  NbCardModule,
  NbCardComponent, NbSelectModule
} from '@nebular/theme';
import { MatDividerModule } from '@angular/material/divider';
import { MatMenuModule } from '@angular/material/menu';
import { FooterComponent } from './footer/footer.component';
import { FontAwesomeModule } from '@fortawesome/angular-fontawesome';
import { FormsModule } from '@angular/forms';
import { MatIconModule } from '@angular/material/icon';
import { BsDropdownModule } from 'ngx-bootstrap/dropdown';
import { MatListModule } from '@angular/material/list';

@NgModule({
  declarations: [HeaderComponent,
    SidenavComponent,
    FooterComponent],
  imports: [
    CommonModule,
    MatMenuModule,
    RouterModule,
    NbIconModule,
    NbLayoutModule,
    NbMenuModule,
    NbSidebarModule,
    MatDividerModule,
    FontAwesomeModule,
    NbToggleModule,
    NbCardModule,
    NbEvaIconsModule,
    FormsModule,
    MatIconModule,
    NbSelectModule,
    BsDropdownModule.forRoot(),
    MatListModule
  ],
  exports: [HeaderComponent,
    SidenavComponent,
    FooterComponent,
    NbToggleComponent,
    NbCardComponent],
})
export class PagesComponentsModule {}
