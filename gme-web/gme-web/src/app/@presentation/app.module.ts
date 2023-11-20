import { BrowserModule, Title } from '@angular/platform-browser';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { APP_INITIALIZER, LOCALE_ID, NgModule } from '@angular/core';
import { HTTP_INTERCEPTORS, HttpClientModule } from '@angular/common/http';
import { ThemeModule } from './@theme/theme.module';
import { NgxSpinnerModule } from 'ngx-spinner';
import { AppComponent } from './app.component';
import { AppRoutingModule } from './app-routing.module';
import {
  NbDatepickerModule,
  NbDialogModule,
  NbIconModule,
  NbMenuModule,
  NbSidebarModule,
  NbThemeModule,
  NbTimepickerModule,
  NbToastrModule,
  NbWindowModule,
} from '@nebular/theme';
import { NbEvaIconsModule } from '@nebular/eva-icons';
import { DomainModule } from '../@domain/domain.module';
import { BasicAuthInterceptor, ErrorInterceptor } from '../@data/interceptors';
import { Const } from '../@data/services/const';
import { JwtModule } from '@auth0/angular-jwt';

import localeEs from '@angular/common/locales/es';
import { registerLocaleData } from '@angular/common';
import { NbDateFnsDateModule } from '@nebular/date-fns';
import { CommonComponentsModule } from './@common-components/common-components.module';
import { FontAwesomeModule } from '@fortawesome/angular-fontawesome';
import { MyHttpInterceptor } from '../@data/interceptors/request.interceptor';
import { HashLocationStrategy, LocationStrategy } from '@angular/common';
import { NgxMaskModule } from 'ngx-mask';

registerLocaleData(localeEs);

@NgModule({
  declarations: [AppComponent],
  imports: [
    BrowserModule,
    BrowserAnimationsModule,
    HttpClientModule,
    AppRoutingModule,
    NgxSpinnerModule,
    NbSidebarModule.forRoot(),
    NbMenuModule.forRoot(),
    NbDatepickerModule.forRoot(),
    NbDialogModule.forRoot(),
    NbWindowModule.forRoot(),
    NbToastrModule.forRoot(),
    NbThemeModule.forRoot({ name: 'corporate' }),
    ThemeModule.forRoot(),
    DomainModule.forRoot(),
    NbTimepickerModule.forRoot(),
    CommonComponentsModule,
    NbDateFnsDateModule.forRoot({ format: 'dd/MM/yyyy' }),
    JwtModule.forRoot({
      config: { tokenGetter },
    }),
    FontAwesomeModule,
    NbEvaIconsModule,
    NbIconModule,
    NgxMaskModule.forRoot({
      showMaskTyped : true,
      // clearIfNotMatch : true
    })
  ],
  providers: [
    {
      provide: APP_INITIALIZER,
      useFactory: initCommonConfig,
      deps: [Const],
      multi: true,
    },
    {
      provide: APP_INITIALIZER,
      useFactory: initEntidadConfig,
      deps: [Const],
      multi: true,
    },
    { provide: LOCALE_ID, useValue: 'es-ES' },
    {
      provide: HTTP_INTERCEPTORS,
      useClass: BasicAuthInterceptor,
      multi: true,
    },
    {
      provide: HTTP_INTERCEPTORS,
      useClass: ErrorInterceptor,
      multi: true,
    },
    {
      provide: HTTP_INTERCEPTORS,
      useClass: MyHttpInterceptor,
      multi: true,
    },
    Title,
    { provide: LocationStrategy, useClass: HashLocationStrategy },
  ],
  bootstrap: [AppComponent],
})
export class AppModule {}

export function tokenGetter() {
  return sessionStorage.getItem('token');
}

export function initCommonConfig(c: Const) {
  return () => c.loadCommonConfig();
}

export function initEntidadConfig(c: Const) {
  return () => c.loadEntidadConfig();
}
