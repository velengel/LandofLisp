#include <stdio.h>
#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>

const int SCREEN_WIDTH  = 640;
const int SCREEN_HEIGHT = 480;

const  int BPP          =  32;

#define  ERROR_REPORT(_message_)                                    \
    fprintf( stderr, "%s:%d %s\n", __FILE__, __LINE__, _message_ )

static void color_assign(struct SDL_Color *self, Uint8 red, Uint8 green, Uint8 blue);

/* static void rect_assign(SDL_Rect *self, int x, int y, int width, int height); */
static void rect_from_surface(SDL_Rect *self, SDL_Surface *surface);
static void rect_centering(SDL_Rect *self, int max_width, int max_height, SDL_Rect *other);

int main(int argc, char **argv) {
    if ( SDL_Init( SDL_INIT_EVERYTHING ) < 0 ) {
        ERROR_REPORT( SDL_GetError() );

        return -1;
    }
    if ( TTF_Init() < 0 ) {
        ERROR_REPORT( SDL_GetError() );

        SDL_Quit();

        return -1;
    }

    SDL_Color    white;
    color_assign( &white, 0xff, 0xff, 0xff );

    SDL_Window  *window      = SDL_CreateWindow( "Hello, TTF",
                                                 SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                                                 SCREEN_WIDTH, SCREEN_HEIGHT,
                                                 SDL_WINDOW_OPENGL );

    SDL_Surface *screen      = SDL_GetWindowSurface( window );

    TTF_Font    *monapo_font = TTF_OpenFont( "/usr/share/fonts/TTF/monapo.ttf", 16 );
    if ( monapo_font == NULL ) {
        ERROR_REPORT( SDL_GetError() );

        TTF_Quit();
        SDL_Quit();

        return -1;
    }

    SDL_Surface *image       =  TTF_RenderUTF8_Blended( monapo_font, "こんにちは！ こんにちは！", white );

    SDL_Rect     image_rect;
    /* rect_assign( &image_rect, 0, 0, image->w, image->h ); */
    rect_from_surface( &image_rect, image );

    printf( "image_rect.w = %d, image_rect.h = %d\n", image_rect.w, image_rect.h );

    SDL_Rect screen_rect;
    /* rect_assign( &screen_rect, (SCREEN_WIDTH / 2) - image_rect.w, (SCREEN_HEIGHT / 2) - image_rect.h, 0, 0 ); */
    rect_centering( &screen_rect , SCREEN_WIDTH, SCREEN_HEIGHT, &image_rect );

    printf( "screen_rect.x = %d, screen_rect.x = %d\n", screen_rect.x, screen_rect.y );

    SDL_BlitSurface( image, &image_rect, screen, &screen_rect );

    SDL_UpdateWindowSurface( window );

    int         be_exit = 1;
    SDL_Event   an_event;
    while ( be_exit ) {
        if ( SDL_PollEvent( &an_event ) ) {
            switch ( an_event.type ) {
                case SDL_KEYDOWN:
                    be_exit = 0;
                    break;

                case SDL_QUIT:
                    be_exit = 0;
                    break;

                default:
                    break;
            }
        }
        SDL_Delay( 1 );
    }

    SDL_FreeSurface( image );
    SDL_DestroyWindow( window );
    TTF_CloseFont( monapo_font );
    TTF_Quit();
    SDL_Quit();

    return 0;
}


static void color_assign(struct SDL_Color *self, Uint8 red, Uint8 green, Uint8 blue) {
    self->r = red;
    self->g = green;
    self->b = blue;
}


/* static void rect_assign(SDL_Rect *self, int x, int y, int width, int height) { */
/*     self->x = x; */
/*     self->y = y; */
/*     self->w = width; */
/*     self->h = height; */
/* } */

static void rect_from_surface(SDL_Rect *self, SDL_Surface *surface) {
    self->x = 0;
    self->y = 0;
    self->w = surface->w;
    self->h = surface->h;
}


static void rect_centering(SDL_Rect *self, int max_width, int max_height, SDL_Rect *other) {
    self->x = (max_width  / 2) - other->w;
    self->y = (max_height / 2) - other->h;
    self->w = 0;
    self->h = 0;
}