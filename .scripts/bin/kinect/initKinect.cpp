#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <libfreenect/libfreenect.hpp>
#include <libfreenect/libfreenect_audio.h>

#ifdef _MSC_VER
    #define HAVE_STRUCT_TIMESPEC
#endif

#ifndef SIGQUIT // win32 compat
    #define SIGQUIT SIGTERM
#endif

volatile int count = 0;
volatile bool running = true;

void depth_cb(freenect_device* dev, void* data, uint32_t timestamp)
{
    printf("Received depth frame at %d\n", timestamp);
}

void video_cb(freenect_device* dev, void* data, uint32_t timestamp)
{
    printf("Received video frame at %d\n", timestamp);
    count++;
    if (count == 20) running = false;
}

void signalHandler(int signal)
{
    if (signal == SIGINT
        || signal == SIGTERM
        || signal == SIGQUIT) running = false;
}

pthread_t freenect_thread;
volatile int die = 0;

int window;

static freenect_context* f_ctx;
static freenect_device* f_dev;

typedef struct {
    int32_t* buffers[4];
    int max_samples;
    int current_idx;  // index to the oldest data in the buffer (equivalently, where the next new data will be placed)
    int new_data;
} capture;

capture state;

int paused = 0;

pthread_mutex_t audiobuf_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t audiobuf_cond = PTHREAD_COND_INITIALIZER;

int win_h, win_w;

void in_callback(freenect_device* dev, int num_samples,
                 int32_t* mic1, int32_t* mic2,
                 int32_t* mic3, int32_t* mic4,
                 int16_t* cancelled, void *unknown) {
    pthread_mutex_lock(&audiobuf_mutex);
    capture* c = (capture*)freenect_get_user(dev);
    if(num_samples < c->max_samples - c->current_idx) {
        memcpy(&(c->buffers[0][c->current_idx]), mic1, num_samples*sizeof(int32_t));
        memcpy(&(c->buffers[1][c->current_idx]), mic2, num_samples*sizeof(int32_t));
        memcpy(&(c->buffers[2][c->current_idx]), mic3, num_samples*sizeof(int32_t));
        memcpy(&(c->buffers[3][c->current_idx]), mic4, num_samples*sizeof(int32_t));
    } else {
        int first = c->max_samples - c->current_idx;
        int left = num_samples - first;
        memcpy(&(c->buffers[0][c->current_idx]), mic1, first*sizeof(int32_t));
        memcpy(&(c->buffers[1][c->current_idx]), mic2, first*sizeof(int32_t));
        memcpy(&(c->buffers[2][c->current_idx]), mic3, first*sizeof(int32_t));
        memcpy(&(c->buffers[3][c->current_idx]), mic4, first*sizeof(int32_t));
        memcpy(c->buffers[0], &mic1[first], left*sizeof(int32_t));
        memcpy(c->buffers[1], &mic2[first], left*sizeof(int32_t));
        memcpy(c->buffers[2], &mic3[first], left*sizeof(int32_t));
        memcpy(c->buffers[3], &mic4[first], left*sizeof(int32_t));
    }
    c->current_idx = (c->current_idx + num_samples) % c->max_samples;
    c->new_data = 1;
    pthread_cond_signal(&audiobuf_cond);
    pthread_mutex_unlock(&audiobuf_mutex);
}

void* freenect_threadfunc(void* arg) {
    int count = 0;
    while(!die && freenect_process_events(f_ctx) >= 0) {
        printf("%d \n", count);
        if (count > 100) die = true;
        count++;
        // If we did anything else in the freenect thread, it might go here.
    }
    freenect_stop_audio(f_dev);
    freenect_close_device(f_dev);
    freenect_shutdown(f_ctx);
    return NULL;
}

int main(int argc, char** argv)
{
    // Handle signals gracefully.
    signal(SIGINT, signalHandler);
    signal(SIGTERM, signalHandler);
    signal(SIGQUIT, signalHandler);

    // Initialize libfreenect.
    freenect_context* fn_ctx;
    int ret = freenect_init(&fn_ctx, NULL);
    if (ret < 0)
        return ret;

    // Show debug messages and use camera only.
    freenect_set_log_level(fn_ctx, FREENECT_LOG_DEBUG);
    freenect_select_subdevices(fn_ctx, FREENECT_DEVICE_CAMERA);

    // Find out how many devices are connected.
    int num_devices = ret = freenect_num_devices(fn_ctx);
    if (ret < 0)
        return ret;
    if (num_devices == 0)
    {
        printf("No device found!\n");
        freenect_shutdown(fn_ctx);
        return 1;
    }

    // Open the first device.
    freenect_device* fn_dev;
    ret = freenect_open_device(fn_ctx, &fn_dev, 0);
    if (ret < 0)
    {
        freenect_shutdown(fn_ctx);
        return ret;
    }

    // Set depth and video modes.
    ret = freenect_set_depth_mode(fn_dev, freenect_find_depth_mode(FREENECT_RESOLUTION_MEDIUM, FREENECT_DEPTH_MM));
    if (ret < 0)
    {
        freenect_shutdown(fn_ctx);
        return ret;
    }
    ret = freenect_set_video_mode(fn_dev, freenect_find_video_mode(FREENECT_RESOLUTION_MEDIUM, FREENECT_VIDEO_RGB));
    if (ret < 0)
    {
        freenect_shutdown(fn_ctx);
        return ret;
    }

    // Set frame callbacks.
    freenect_set_depth_callback(fn_dev, depth_cb);
    freenect_set_video_callback(fn_dev, video_cb);

    // Start depth and video.
    ret = freenect_start_depth(fn_dev);
    if (ret < 0)
    {
        freenect_shutdown(fn_ctx);
        return ret;
    }
    ret = freenect_start_video(fn_dev);
    if (ret < 0)
    {
        freenect_shutdown(fn_ctx);
        return ret;
    }

    while (running && freenect_process_events(fn_ctx) >= 0)
    {

    }

    printf("Shutting down\n");

    // Stop everything and shutdown.
    freenect_stop_depth(fn_dev);
    freenect_stop_video(fn_dev);
    freenect_close_device(fn_dev);
    freenect_shutdown(fn_ctx);

    printf("Done!\n");

    // Switch to microphone test

    if (freenect_init(&f_ctx, NULL) < 0) {
        printf("freenect_init() failed\n");
        return 1;
    }
    freenect_set_log_level(f_ctx, FREENECT_LOG_INFO);
    freenect_select_subdevices(f_ctx, FREENECT_DEVICE_AUDIO);

    int nr_devices = freenect_num_devices (f_ctx);
    printf ("Number of devices found: %d\n", nr_devices);
    if (nr_devices < 1) {
        freenect_shutdown(f_ctx);
        return 1;
    }

    int user_device_number = 0;
    while (freenect_open_device(f_ctx, &f_dev, user_device_number) < 0) {
        printf("Could not open device\n");
        //freenect_shutdown(f_ctx);
        //return 1;
    }

    state.max_samples = 256 * 60;
    state.current_idx = 0;
    state.buffers[0] = (int32_t*)malloc(state.max_samples * sizeof(int32_t));
    state.buffers[1] = (int32_t*)malloc(state.max_samples * sizeof(int32_t));
    state.buffers[2] = (int32_t*)malloc(state.max_samples * sizeof(int32_t));
    state.buffers[3] = (int32_t*)malloc(state.max_samples * sizeof(int32_t));
    memset(state.buffers[0], 0, state.max_samples * sizeof(int32_t));
    memset(state.buffers[1], 0, state.max_samples * sizeof(int32_t));
    memset(state.buffers[2], 0, state.max_samples * sizeof(int32_t));
    memset(state.buffers[3], 0, state.max_samples * sizeof(int32_t));
    freenect_set_user(f_dev, &state);

    freenect_set_audio_in_callback(f_dev, in_callback);
    freenect_start_audio(f_dev);

    int res = pthread_create(&freenect_thread, NULL, freenect_threadfunc, NULL);
    if (res) {
        printf("pthread_create failed\n");
        freenect_shutdown(f_ctx);
        return 1;
    }
    printf("This is the libfreenect microphone waveform viewer.  Press 'q' to quit or spacebar to pause/unpause the view.\n");

    while (!die);

    return 0;
}
